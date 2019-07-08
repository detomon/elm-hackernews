module HackerNews exposing
    ( Msg (..), Item (..)
    , Story, Comment
    , pageUrl
    , fetchTopStories
    , fetchItems
    , itemId
    )


{-| Fetch Hackernews stories and comments.

@docs Msg, Item, Story, Comment


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
    = GotTopStories (Result Http.Error (List Int))
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
