module HackerNews exposing
    ( Model, Item(..), ItemId, Story, Comment, Msg(..)
    , empty, update, setPage, currentItems, currentComments, pagesCount
    , fetchTopStories, fetchItems, fetchChildren
    , itemId, pageUrl
    )

{-| Fetch Hackernews stories and comments.

@docs Model, Item, ItemId, Story, Comment, Msg


# Model

@docs empty, update, setPage, currentItems, currentComments, pagesCount


# Network

@docs fetchTopStories, fetchItems, fetchChildren


# Helpers

@docs itemId, pageUrl

-}

import Dict
import Http
import Json.Decode as D
import Json.Decode.Pipeline as JP
import MultiwayTree as MT
import MultiwayTree.Extra as MTE
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
    { allItemIds : List ItemId
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


{-| Known item types.
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
apiUrl : String
apiUrl =
    "https://hacker-news.firebaseio.com/v0"


{-| The top stories URL.
-}
topStoriesUrl : String
topStoriesUrl =
    apiUrl ++ "/topstories.json"


{-| Make item URL with ID.
-}
itemUrl : ItemId -> String
itemUrl id =
    apiUrl ++ "/item/" ++ String.fromInt id ++ ".json"


{-| Http request timeout.
-}
httpTimeout : Float
httpTimeout =
    60 * 1000



-- MAIN


{-| Get empty model.
-}
empty : Int -> Model
empty itemsPerPage =
    { allItemIds = []
    , pagedItems = []
    , comments = emptyComments
    , itemsCache = Dict.empty
    , itemsPerPage = itemsPerPage
    , page = 0
    , error = Nothing
    }


emptyComments : MT.Tree Item
emptyComments =
    MT.Tree (ItemPlaceholder 0) []


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
    (List.length model.allItemIds + model.itemsPerPage - 1) // model.itemsPerPage


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
        placeholderId item =
            case item of
                ItemPlaceholder id ->
                    Just id

                _ ->
                    Nothing

        replacePlaceholder map id newItem =
            map
                (\item ->
                    if placeholderId item == Just id then
                        newItem

                    else
                        item
                )

        setError err =
            ( { model | error = Just <| resultErrorString err }, Cmd.none )
    in
    case msg of
        GotTopStories result ->
            case result of
                Ok ids ->
                    setPage model.page { model | allItemIds = ids }

                Err err ->
                    setError err

        GotItem id result ->
            case result of
                Ok item ->
                    let
                        isNotDeleted comment =
                            case comment of
                                ItemComment { deleted } ->
                                    not deleted

                                _ ->
                                    True

                        addChildren (MT.Tree child _) =
                            let
                                makePlaceholder childId =
                                    MT.Tree (ItemPlaceholder childId) []

                                kids =
                                    itemKids item
                            in
                            if itemId child == id then
                                MT.Tree child (List.map makePlaceholder kids)
                                    |> Just

                            else
                                Nothing

                        newModel =
                            { model
                                | pagedItems = replacePlaceholder List.map id item model.pagedItems
                                , comments =
                                    replacePlaceholder MT.map id item model.comments
                                        |> MT.filter isNotDeleted
                                        |> Maybe.withDefault emptyComments
                                        |> MTE.replace addChildren
                                , itemsCache = Dict.insert id item model.itemsCache
                            }

                        ( _, loadChildren ) =
                            case item of
                                ItemComment _ ->
                                    fetchItems (itemKids item) newModel

                                _ ->
                                    ( [], Cmd.none )
                    in
                    ( newModel, loadChildren )

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


{-| Get kids IDs.
-}
itemKids : Item -> List ItemId
itemKids item =
    case item of
        ItemStory { kids } ->
            kids

        ItemComment { kids } ->
            kids

        _ ->
            []


{-| Set page.
-}
setPage : Int -> Model -> ( Model, Cmd Msg )
setPage page model =
    let
        newPage =
            max 0 page

        pagedItemsIds =
            model.allItemIds
                |> paging model.itemsPerPage newPage

        ( pagedItems, cmd ) =
            fetchItems pagedItemsIds model

        newModel =
            { model
                | page = newPage
                , pagedItems = pagedItems
            }
    in
    ( newModel, cmd )



-- NETWORK


httpGet :
    { url : String
    , expect : Http.Expect msg
    }
    -> Cmd msg
httpGet r =
    Http.request
        { method = "GET"
        , headers = []
        , url = r.url
        , body = Http.emptyBody
        , expect = r.expect
        , timeout = Just httpTimeout
        , tracker = Nothing
        }


{-| Decode item with type name.
-}
decodeItemWithType : String -> D.Decoder Item
decodeItemWithType type_ =
    let
        timeFromInt =
            D.int |> D.map (\seconds -> Time.millisToPosix (seconds * 1000))
    in
    case type_ of
        "story" ->
            D.succeed Story
                |> JP.required "by" D.string
                |> JP.required "descendants" D.int
                |> JP.required "id" D.int
                |> JP.optional "kids" (D.list D.int) []
                |> JP.required "score" D.int
                |> JP.required "time" timeFromInt
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
                |> JP.required "time" timeFromInt
                |> JP.optional "deleted" D.bool False
                |> D.map ItemComment

        "job" ->
            D.succeed Job
                |> JP.required "by" D.string
                |> JP.required "id" D.int
                |> JP.required "score" D.int
                |> JP.required "time" timeFromInt
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
    httpGet
        { url = topStoriesUrl
        , expect = Http.expectJson GotTopStories decodeItemIds
        }


{-| Fetch cached items or fetch new items. Add placeholder if not found and add fetch command.
-}
fetchItems : List ItemId -> Model -> ( List Item, Cmd Msg )
fetchItems itemIds model =
    let
        decodeItem =
            D.field "type" D.string
                |> D.andThen decodeItemWithType

        fetchItem msg id =
            httpGet
                { url = itemUrl id
                , expect = Http.expectJson msg decodeItem
                }

        fetchList =
            List.map (\id -> fetchItem (GotItem id) id)
                >> List.reverse
                -- reverse; commands seem to be started backwards
                >> Cmd.batch

        cachedOrPlaceholderItem id =
            Dict.get id model.itemsCache
                |> Maybe.withDefault (ItemPlaceholder id)

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
            fetchList missingItemIds
    in
    ( itemList, cmd )


{-| Fetch children recursive from item.
-}
fetchChildren : ItemId -> Model -> ( Model, Cmd Msg )
fetchChildren parentId model =
    let
        parent =
            Dict.get parentId model.itemsCache
                |> Maybe.withDefault (ItemPlaceholder 0)

        fetchChild child =
            let
                ( children, subCmd ) =
                    fetchItems (itemKids child) model

                ( treeChildren, childCmds ) =
                    children
                        |> List.map fetchChild
                        |> List.unzip

                cmds =
                    subCmd :: childCmds
            in
            ( MT.Tree child treeChildren, Cmd.batch cmds )

        ( comments, cmdList ) =
            fetchChild parent
    in
    ( { model | comments = comments }, cmdList )
