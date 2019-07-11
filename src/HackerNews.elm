module HackerNews exposing
    ( Model, Item(..), Story, Comment, Msg(..)
    , empty, update, setPage, currentItems, currentComments, pagesCount
    , fetchTopStories, fetchItems, fetchComments
    , itemId, pageUrl
    , ItemId
    )

{-| Fetch Hackernews stories and comments.

@docs Model, Item, ItemId, Story, Comment, Msg


# Model

@docs empty, update, setPage, currentItems, currentComments, pagesCount


# Network

@docs fetchTopStories, fetchItems, fetchComments


# Helpers

@docs itemId, pageUrl

-}

import Dict
import Http
import Json.Decode as D
import Json.Decode.Pipeline as JP
import MultiwayTree as MT
import Time



-- TYPES


{-| Messages.
-}
type Msg
    = GotTopStories (Result Http.Error (List Int))
    | GotItem Int (Result Http.Error Item)


{-| Item ID type.
-}
type alias ItemId =
    Int


{-| Model.
-}
type alias Model =
    { allIitemIds : List ItemId
    , pagedItems : List Item
    , comments : MT.Tree Item
    , itemsCache : Dict.Dict ItemId Item
    , itemsPerPage : Int
    , page : Int
    , error : Maybe String
    }


{-| Story item.
-}
type alias Story =
    { by : String
    , descendants : Int
    , id : ItemId
    , kids : List Int
    , score : Int
    , time : Time.Posix
    , title : String
    , url : Maybe String
    }


{-| Comment item.
-}
type alias Comment =
    { by : String
    , id : ItemId
    , kids : List Int
    , parent : ItemId
    , text : String
    , time : Time.Posix
    , deleted : Bool
    }


{-| Job item.
-}
type alias Job =
    { by : String
    , id : ItemId
    , score : Int
    , time : Time.Posix
    , title : String
    , url : Maybe String
    }


{-| Allowed item types.
-}
type Item
    = ItemPlaceholder ItemId
    | ItemStory Story
    | ItemComment Comment
    | ItemJob Job



-- API URLS


{-| The main site URL.
-}
pageUrl : String
pageUrl =
    "https://news.ycombinator.com"


{-| The base API URL.
-}
baseUrl : String
baseUrl =
    "https://hacker-news.firebaseio.com/v0"


{-| The top stories URL.
-}
topStoriesUrl : String
topStoriesUrl =
    baseUrl ++ "/topstories.json"


{-| Make item URL with ID.
-}
itemUrl : ItemId -> String
itemUrl id =
    baseUrl ++ "/item/" ++ String.fromInt id ++ ".json"



-- MAIN


{-| Get empty model.
-}
empty : Int -> Model
empty itemsPerPage =
    { allIitemIds = []
    , pagedItems = []
    , comments = MT.Tree (ItemPlaceholder 0) []
    , itemsCache = Dict.empty
    , itemsPerPage = itemsPerPage
    , page = 0
    , error = Nothing
    }


{-| Create a paged list segment.
-}
paging : Int -> Int -> List a -> List a
paging itemsPerPage page =
    List.drop (itemsPerPage * page)
        >> List.take itemsPerPage


{-| Get number of pages.
-}
pagesCount : Model -> Int
pagesCount model =
    (List.length model.allIitemIds + model.itemsPerPage - 1) // model.itemsPerPage


{-| Get error string from Http error.
-}
resultErrorString : Http.Error -> String
resultErrorString error =
    case error of
        Http.BadStatus status ->
            "BadStatus: " ++ String.fromInt status

        Http.BadUrl url ->
            "BadUrl: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadBody str ->
            "BadBody: " ++ str


{-| Update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        replacePlaceholder map id newItem =
            map
                (\item ->
                    case item of
                        ItemPlaceholder placeholderId ->
                            if placeholderId == id then
                                newItem

                            else
                                item

                        _ ->
                            item
                )

        setError err =
            ( { model | error = Just <| resultErrorString err }, Cmd.none )
    in
    case msg of
        GotTopStories result ->
            case result of
                Ok ids ->
                    setPage model.page { model | allIitemIds = ids }

                Err err ->
                    setError err

        GotItem id result ->
            case result of
                Ok item ->
                    let
                        removeDeletedComment comment =
                            case comment of
                                ItemComment { deleted } ->
                                    not deleted

                                _ ->
                                    True

                        newModel =
                            { model
                                | pagedItems = replacePlaceholder List.map id item model.pagedItems
                                , comments =
                                    replacePlaceholder MT.map id item model.comments
                                        |> MT.filter removeDeletedComment
                                        |> Maybe.withDefault (MT.Tree (ItemPlaceholder 0) [])
                                , itemsCache = Dict.insert id item model.itemsCache
                            }

                        loadChildComments =
                            case item of
                                ItemComment { kids } ->
                                    fetchItems kids

                                _ ->
                                    Cmd.none
                    in
                    ( newModel, loadChildComments )

                Err err ->
                    setError err


{-| Get item ID.
-}
itemId : Item -> ItemId
itemId item =
    case item of
        ItemPlaceholder id ->
            id

        ItemStory { id } ->
            id

        ItemComment { id } ->
            id

        ItemJob { id } ->
            id


{-| Get paged items.
-}
currentItems : Model -> List Item
currentItems { pagedItems } =
    pagedItems


{-| Get current comments.
-}
currentComments : Model -> MT.Tree Item
currentComments { comments } =
    comments


{-| Get kids IDs using model cache.
-}
itemKids : Model -> ItemId -> List ItemId
itemKids { itemsCache } id =
    case Dict.get id itemsCache of
        Just item ->
            case item of
                ItemStory { kids } ->
                    kids

                ItemComment { kids } ->
                    kids

                _ ->
                    []

        Nothing ->
            []


{-| Set page.
-}
setPage : Int -> Model -> ( Model, Cmd Msg )
setPage page model =
    let
        pagedItemsIds =
            model.allIitemIds
                |> paging model.itemsPerPage page

        ( pagedItems, cmd ) =
            getItems model pagedItemsIds

        newModel =
            { model
                | page = page
                , pagedItems = pagedItems
            }
    in
    ( newModel, cmd )



-- NETWORK


{-| Fetch item with ID.
-}
fetchItem : (Result Http.Error Item -> Msg) -> ItemId -> Cmd Msg
fetchItem msg id =
    let
        decodeItem =
            D.field "type" D.string
                |> D.andThen decodeItemWithType
    in
    Http.get
        { url = itemUrl id
        , expect = Http.expectJson msg decodeItem
        }


{-| Decode item with type name.
-}
decodeItemWithType : String -> D.Decoder Item
decodeItemWithType type_ =
    let
        timeFromSeconds seconds =
            Time.millisToPosix (seconds * 1000)
    in
    case type_ of
        "story" ->
            D.succeed Story
                |> JP.required "by" D.string
                |> JP.required "descendants" D.int
                |> JP.required "id" D.int
                |> JP.optional "kids" (D.list D.int) []
                |> JP.required "score" D.int
                |> JP.required "time" (D.int |> D.map timeFromSeconds)
                |> JP.required "title" D.string
                |> JP.optional "url" (D.string |> D.map Just) Nothing
                |> D.map ItemStory

        "comment" ->
            D.succeed Comment
                |> JP.optional "by" D.string ""
                |> JP.required "id" D.int
                |> JP.optional "kids" (D.list D.int) []
                |> JP.required "parent" D.int
                |> JP.optional "text" D.string ""
                |> JP.custom (D.field "time" D.int |> D.map timeFromSeconds)
                |> JP.optional "deleted" D.bool False
                |> D.map ItemComment

        "job" ->
            D.succeed Job
                |> JP.required "by" D.string
                |> JP.required "id" D.int
                |> JP.required "score" D.int
                |> JP.required "time" (D.int |> D.map timeFromSeconds)
                |> JP.required "title" D.string
                |> JP.optional "url" (D.string |> D.map Just) Nothing
                |> D.map ItemJob

        _ ->
            D.fail ("Unknown type '" ++ type_ ++ "'")


{-| Fetch top stories.
-}
fetchTopStories : Cmd Msg
fetchTopStories =
    let
        decodeItemIds =
            D.list D.int
    in
    Http.get
        { url = topStoriesUrl
        , expect = Http.expectJson GotTopStories decodeItemIds
        }


{-| Fetch items with given IDs.
-}
fetchItems : List ItemId ->  Cmd Msg
fetchItems =
    List.map (\id -> fetchItem (GotItem id) id)
        >> List.reverse
        -- reverse; commands seem to be started backwards
        >> Cmd.batch


{-| Fetch cached items. Add placeholder if not found and add fetch command.
-}
getItems : Model -> List ItemId -> ( List Item, Cmd Msg )
getItems model itemIds =
    let
        cachedOrPlaceholderItem id =
            case Dict.get id model.itemsCache of
                Just item ->
                    item

                Nothing ->
                    ItemPlaceholder id

        isPlaceholder item =
            case item of
                ItemPlaceholder _ ->
                    True

                _ ->
                    False

        itemList =
            List.map cachedOrPlaceholderItem itemIds

        missingItemIds =
            itemList
                |> List.filter isPlaceholder
                |> List.map itemId

        cmd =
            fetchItems missingItemIds
    in
    ( itemList, cmd )


{-| Fetch comments from (already loaded) parent item.
-}
fetchComments : Model -> ItemId -> ( Model, Cmd Msg )
fetchComments model parentId =
    let
        item =
            case Dict.get parentId model.itemsCache of
                Just i ->
                    i

                Nothing ->
                    ItemPlaceholder 0

        ( comments, cmd ) =
            getItems model (itemKids model parentId)

        newModel =
            { model
                | comments = MT.Tree item (List.map (\i -> MT.Tree i []) comments)
            }
    in
    ( newModel, cmd )
