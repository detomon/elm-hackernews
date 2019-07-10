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
import List.Extra as ListExtra
import Markdown
import Markdown.Config as MarkdownConfig
import MultiwayTree as MT
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
    | ShowComments HN.ItemId


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
    let
        updateHackernews newModel (hackernews, cmd) =
            ( { newModel | hackernews = hackernews }, Cmd.map HackerNewsMsg cmd )
    in
    case msg of
        -- fetch top stories after retrieving time zone
        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.map HackerNewsMsg HN.fetchTopStories )

        HackerNewsMsg submsg ->
            HN.update submsg model.hackernews |> updateHackernews model

        UpdatePage page ->
            HN.setPage page model.hackernews |> updateHackernews model

        ShowComments itemId ->
            HN.fetchComments model.hackernews itemId |> updateHackernews model


-- VIEW


{-| Conditionally render node
-}
nodeIf : String -> List (H.Attribute msg) -> List (H.Html msg) -> Bool -> H.Html msg
nodeIf name attrs children flag =
    if flag then
        H.node name attrs children
    else
        H.text ""


itemKeyed : HN.Item -> (String, H.Html Msg)
itemKeyed item =
    ( String.fromInt (HN.itemId item), Lazy.lazy viewPost item )


view : Model -> Browser.Document Msg
view model =
    let
        items =
            HN.currentItems model.hackernews
                |> List.map itemKeyed
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
            , Keyed.node "ol"
                [ A.class "post-list"
                , A.start (model.hackernews.page * itemsPerPage + 1)
                ]
                items
            , viewPaging model
            , viewComments model
            ]
        ]
    }


viewPaging : Model -> H.Html Msg
viewPaging model =
    let
        pagesCount =
            HN.pagesCount model.hackernews

        pageItem n =
            H.li
                [ A.class "paging__page"
                , A.classList [("paging--active", model.hackernews.page == n)]
                , E.onClick (UpdatePage n)
                ]
                [ H.a [] [ H.text (String.fromInt (n + 1)) ]
                ]
    in
    H.ul [ A.class "paging" ] (ListExtra.initialize pagesCount pageItem)


viewComments : Model -> H.Html Msg
viewComments model =
    let
        comments =
            HN.currentComments model.hackernews
                |> MT.flatten
                |> List.map itemKeyed
    in
    H.div [ A.class "comments-wrapper" ]
        [ Keyed.node "ul" [ A.class "comments-list" ] comments ]


viewPost : HN.Item -> H.Html Msg
viewPost post =
    let
        markdownOptions =
            let
                options =
                    MarkdownConfig.defaultOptions
            in
            { options
                | rawHtml = MarkdownConfig.ParseUnsafe
            }
    in
    case post of
        HN.ItemPlaceholder id ->
            viewPostTitle "post--placeholder" (" ") [ H.text (" ") ] Nothing

        HN.ItemStory story ->
            let
                info =
                    [ H.text (String.fromInt story.score
                        ++ " points by " ++ story.by ++ " | ")
                    , H.a [ E.onClick (ShowComments story.id) ]
                        [ H.text (String.fromInt story.descendants
                            ++ " comments")
                        ]
                    ]
            in
            viewPostTitle "" story.title info story.url

        HN.ItemComment comment ->
            let
                info =
                    [ H.text ("by " ++ comment.by ++ " | ")
                    ]

                text =
                    "<div>" ++ comment.text ++ "</div>"

            in
            H.li []
                (Markdown.toHtml (Just markdownOptions) text)

        HN.ItemJob job ->
            let
                infoText =
                    String.fromInt job.score
                        ++ " points by " ++ job.by
            in
            viewPostTitle "" job.title [ H.text infoText ] job.url


viewPostTitle : String -> String -> List (H.Html Msg) -> Maybe String -> H.Html Msg
viewPostTitle class title info href =
    H.li [ A.class "post", A.class class ]
        [ H.span [ A.class "post-title" ]
            [ case href of
                Nothing  -> H.text title
                Just url -> H.a [ A.href url, A.rel "noreferrer" ] [ H.text title ]
            ]
        , H.span [ A.class "post-info" ] info
        ]
