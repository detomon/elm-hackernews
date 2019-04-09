module HackerNews exposing
    ( Msg (..), Item (..)
    , Story, Comment
    , update
    , fetchTopStories
    , itemId
    )

import Http
import Json.Decode as D
import Json.Decode.Pipeline as JP
import Time


-- TYPES


type Msg
    = GotTopStories Int (Result Http.Error (List Int))
    | GotItem Int (Result Http.Error Item)

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

type alias Comment =
    { by : String
    , id : Int
    , kids : List Int
    , parent : Int
    , text : String
    , time : Time.Posix
    }

type Item
    = ItemStory Story
    | ItemComment Comment


-- API URLS


baseUrl : String
baseUrl =
    "https://hacker-news.firebaseio.com/v0"

topStoriesUrl : String
topStoriesUrl =
    baseUrl ++ "/topstories.json"

itemUrl : Int -> String
itemUrl id =
    baseUrl ++ "/item/" ++ String.fromInt id ++ ".json"


-- MAIN


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


-- HELPER FUNCTIONS


timeFromSeconds : Int -> Time.Posix
timeFromSeconds seconds =
    Time.millisToPosix (seconds * 1000)

itemId : Item -> Int
itemId item =
    case item of
        ItemStory story ->
            story.id
        ItemComment comment ->
            comment.id


-- NETWORK


fetchTopStoriesGet : (Result Http.Error (List Int) -> Msg) -> Cmd Msg
fetchTopStoriesGet msg =
    Http.get
        { url = topStoriesUrl
        , expect = Http.expectJson msg decodeItemIds
        }

fetchItem : (Result Http.Error Item -> Msg) -> Int -> Cmd Msg
fetchItem msg id =
    Http.get
        { url = itemUrl id
        , expect = Http.expectJson msg decodeItem
        }

decodeItemIds : D.Decoder (List Int)
decodeItemIds =
    D.list D.int

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

fetchTopStories : Int -> Cmd Msg
fetchTopStories limit =
    fetchTopStoriesGet (GotTopStories limit)
