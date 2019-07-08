module HackerNews exposing
    ( Model, Item (..)
    , Msg (..)
    , Story, Comment
    , empty
    , update
    , pageUrl
    , fetchTopStories
    , fetchItems
    , itemId
    , pagesCount
    , setPage
    )


{-| Fetch Hackernews stories and comments.

@docs Model, Item, Story, Comment, Msg


# Model

@docs empty, update, setPage


# Network

@docs fetchTopStories, fetchItems


# Helpers

@docs itemId, pageCount

-}


import Dict
import Http
import Json.Decode as D
import Json.Decode.Pipeline as JP
import Time


-- TYPES


{-| Messages.
-}
type Msg
    = GotTopStories (Result Http.Error (List Int))
    | GotItem Int (Result Http.Error Item)
    --| UpdatePage Int


{-| Story item.
-}
type alias Story =
    { by : String
    , descendants : Int
    , id : Int
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
    , id : Int
    , kids : List Int
    , parent : Int
    , text : String
    , time : Time.Posix
    }


{-| Allowed item types.
-}
type Item
    = ItemStory Story
    | ItemComment Comment
    | ItemPlaceholder Int
    | ItemError Int String


{-| Model.
-}
type alias Model =
    { items : List Item
    , itemIds : List Int
    , itemsCache : Dict.Dict Int Item
    , itemsPerPage : Int
    , page : Int
    , error : Maybe String
    }


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
itemUrl : Int -> String
itemUrl id =
    baseUrl ++ "/item/" ++ String.fromInt id ++ ".json"


-- MAIN


{-| Get empty model.
-}
empty : Int -> Model
empty itemsPerPage =
    { items = []
    , itemIds = []
    , itemsCache = Dict.empty
    , itemsPerPage = itemsPerPage
    , page = 0
    , error = Nothing
    }


{-| Replace placeholder item with given ID with new item.
-}
replacePlaceholder : Int -> Item -> List Item -> List Item
replacePlaceholder id newItem =
    List.map (\item ->
        case item of
            ItemPlaceholder placeholderId ->
                if placeholderId == id then newItem else item

            _ -> item
    )


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
    (List.length model.itemIds + model.itemsPerPage - 1) // model.itemsPerPage


{-|
-}
resultErrorString : Http.Error -> String
resultErrorString error =
    case error of
        Http.BadStatus status -> "BadStatus: " ++ String.fromInt status
        Http.BadUrl url       -> "BadUrl: " ++ url
        Http.Timeout          -> "Timeout"
        Http.NetworkError     -> "NetworkError"
        Http.BadBody str      -> "BadBody: " ++ str


{-| Update.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTopStories result ->
            case result of
                Ok ids ->
                    let
                        (newModel, cmd) =
                            setPage model.page { model | itemIds = ids }
                    in
                    ( newModel, cmd )

                Err err ->
                    ( { model | error = Just <| resultErrorString err }, Cmd.none )

        GotItem id result ->
            case result of
                Ok item ->
                    ( { model
                        | items = replacePlaceholder id item model.items
                        , itemsCache = Dict.insert id item model.itemsCache
                    }, Cmd.none )

                Err err ->
                    ( { model | error = Just <| resultErrorString err }, Cmd.none )


{-| Get ID of item.
-}
itemId : Item -> Int
itemId item =
    case item of
        ItemStory story ->
            story.id

        ItemComment comment ->
            comment.id

        ItemPlaceholder id ->
            id

        ItemError id err ->
            id

setPage : Int -> Model -> ( Model, Cmd Msg )
setPage page model =
    let
        getItem id =
            case Dict.get id model.itemsCache of
                Just item ->
                    item

                Nothing ->
                    ItemPlaceholder id

        itemList =
            model.itemIds
                |> paging model.itemsPerPage page
                |> List.map getItem

        isPlaceholder item =
            case item of
                ItemPlaceholder _ ->
                    True

                _ ->
                    False

        loadItems =
            itemList
                |> List.filter isPlaceholder 
                |> List.map itemId

        newModel =
            { model
                | page = page
                , items = itemList
            }
    in
    ( newModel, fetchItems loadItems )


-- NETWORK


{-| Fetch item with ID.
-}
fetchItem : (Result Http.Error Item -> Msg) -> Int -> Cmd Msg
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
                |> JP.required "by" D.string
                |> JP.required "id" D.int
                |> JP.optional "kids" (D.list D.int) []
                |> JP.required "parent" D.int
                |> JP.required "text" D.string
                |> JP.custom (D.field "time" D.int |> D.map timeFromSeconds)
                |> D.map ItemComment

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
fetchItems : List Int -> Cmd Msg
fetchItems =
    List.map (\id -> fetchItem (GotItem id) id)
        >> Cmd.batch
