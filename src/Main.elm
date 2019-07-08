module Main exposing (main)


import Browser
import Dict
import HackerNews as HN
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Http
import Json.Decode as Decode
import Task
import Time


type alias Model =
    { title : String
    , timeZone : Time.Zone
    , hackernews : HN.Model
    }


type Item
    = ItemPlaceholder Int
    | ItemError Int String
    | Item HN.Item


type Msg
    = GotTimeZone Time.Zone
    | HackerNewsMsg HN.Msg
    | UpdatePage Int


-- SETTINGS


itemsPerPage : Int
itemsPerPage =
    30


-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> (Model, Cmd Msg)
init flags =
    let
        model =
            { title = "Hacker News"
            , timeZone = Time.utc
            , hackernews = HN.empty itemsPerPage
            }
    in
    ( model, Task.perform GotTimeZone Time.here )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- fetch top stories after retrieving time zone
        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.map HackerNewsMsg HN.fetchTopStories )

        HackerNewsMsg submsg ->
            let
                (hackernews, submsg2) =
                    HN.update submsg model.hackernews
            in
            ( { model | hackernews = hackernews }, Cmd.map HackerNewsMsg submsg2 )

        UpdatePage page ->
            let
                (hackernews, submsg) =
                    HN.setPage page model.hackernews
            in
            ( { model | hackernews = hackernews }, Cmd.map HackerNewsMsg submsg )


for : (Int -> a) -> Int -> List a
for fn count =
    let
        item n =
            if n < count then
                fn n :: item (n + 1)
            else
                []
    in
    item 0


-- VIEW


{-| Conditionally render node
-}
nodeIf : String -> List (H.Attribute msg) -> List (H.Html msg) -> Bool -> H.Html msg
nodeIf name attrs children flag =
    if flag then
        H.node name attrs children
    else
        H.text ""


view : Model -> Browser.Document Msg
view model =
    let
        itemKeyed item =
            ( String.fromInt (HN.itemId item), Lazy.lazy viewPost item )

        page n =
            H.li
                [ A.class "paging__page"
                , A.classList [("paging--active", model.hackernews.page == n)]
                , E.onClick (UpdatePage n)
                ]
                [ H.a [] [ H.text (String.fromInt (n + 1)) ]
                ]
    in
    { title = model.title
    , body =
        [ H.node "link" [ A.rel "stylesheet", A.href "base.css" ] []
        , H.div [ A.class "page-wrapper" ]
            [ H.h1 [ A.class "page-title" ]
                [ H.a [ A.href HN.pageUrl, A.rel "noreferrer" ] [ H.text model.title ]
                ]
            , nodeIf "div" [ A.class "error-message" ]
                [ H.text (model.hackernews.error |> Maybe.withDefault "") ]
                (model.hackernews.error /= Nothing)
            , Keyed.node "ol" [ A.class "post-list", A.start (model.hackernews.page * itemsPerPage + 1) ]
                (List.map itemKeyed model.hackernews.items)
            , H.ul [ A.class "paging" ]
                (for page <| HN.pagesCount model.hackernews)
            ]
        ]
    }


viewPost : HN.Item -> H.Html Msg
viewPost post =
    case post of
        HN.ItemPlaceholder id ->
            viewPostTitle "post--placeholder" (" ") (" ") Nothing

        HN.ItemError id error ->
            viewPostTitle "post--error" error (" ") Nothing

        HN.ItemStory story ->
            let
                infoText =
                    String.fromInt story.score
                        ++ " points by " ++ story.by ++ " | "
                        ++ String.fromInt story.descendants
                        ++ " comments"
            in
            viewPostTitle "" story.title infoText story.url

        HN.ItemComment _ ->
            H.text ""


viewPostTitle : String -> String -> String -> Maybe String -> H.Html Msg
viewPostTitle class title info href =
    H.li [ A.class "post", A.class class ]
        [ H.span [ A.class "post-title" ]
            [ case href of
                Nothing  -> H.text title
                Just url -> H.a [ A.href url, A.rel "noreferrer" ] [ H.text title ]
            ]
        , H.span [ A.class "post-info" ]
            [ H.text info ]
        ]
