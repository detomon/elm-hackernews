module HackerNews exposing
    ( Msg (..), Item (..)
    , Story, Comment
    , update
    , fetchTopStories
    , itemId
    )


{-| Fetch Hackernews stories and comments.

@docs Msg, Item, Story, Comment


# Update

@docs update


# Network

@docs fetchTopStories


# Helpers

@docs itemId

-}


import Http
import Json.Decode as D
import Json.Decode.Pipeline as JP
import Time


-- TYPES


{-| Messages.
-}
type Msg
    = GotTopStories Int (Result Http.Error (List Int))
    | GotItem Int (Result Http.Error Item)


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


-- API URLS


{-| The base URL.
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


{-| Update.
-}
update : Msg -> Cmd Msg
update msg =
    case msg of
        GotTopStories limit result ->
            case result of
                Ok ids ->
                    ids
                        |> List.take limit
                        |> List.map (\id -> fetchItem (GotItem id) id)
                        |> Cmd.batch

                Err error ->
                    -- @todo: forward error
                    Cmd.none

        GotItem id result ->
            case result of
                Ok item ->
                    Cmd.none

                Err error ->
                    -- @todo: forward error
                    Cmd.none


-- HELPERS


{-| Create Time.Posix from seconds.
-}
timeFromSeconds : Int -> Time.Posix
timeFromSeconds seconds =
    Time.millisToPosix (seconds * 1000)


{-| Get ID of item.
-}
itemId : Item -> Int
itemId item =
    case item of
        ItemStory story ->
            story.id

        ItemComment comment ->
            comment.id


-- NETWORK


{-| Fetch ID of top stories.
-}
fetchTopStoriesGet : (Result Http.Error (List Int) -> Msg) -> Cmd Msg
fetchTopStoriesGet msg =
    Http.get
        { url = topStoriesUrl
        , expect = Http.expectJson msg decodeItemIds
        }


{-| Fetch item with ID.
-}
fetchItem : (Result Http.Error Item -> Msg) -> Int -> Cmd Msg
fetchItem msg id =
    Http.get
        { url = itemUrl id
        , expect = Http.expectJson msg decodeItem
        }


{-| Decode list of item IDs.
-}
decodeItemIds : D.Decoder (List Int)
decodeItemIds =
    D.list D.int


{-| Decode result item.
-}
decodeItem : D.Decoder Item
decodeItem =
    let
        decodeType type_ =
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
    in
    D.field "type" D.string
        |> D.andThen decodeType


{-| Fetch top stories with limit.
-}
fetchTopStories : Int -> Cmd Msg
fetchTopStories limit =
    fetchTopStoriesGet (GotTopStories limit)
