module Main exposing (main)

import Browser
import Browser.Events as BE
import Browser.Navigation as Nav
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
import Url
import Url.Parser as UP exposing (Parser, (</>))


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , title : String
    , timeZone : Time.Zone
    , hackernews : HN.Model
    , showComments : Bool
    }


type Item
    = ItemPlaceholder Int
    | ItemError Int String
    | Item HN.Item


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | GotTimeZone Time.Zone
    | HackerNewsMsg HN.Msg
    | UpdatePage Int
    | ShowComments HN.ItemId
    | HideComments
    | KeyDown Int

type Route
    = PageRoute Int



-- SETTINGS


itemsPerPage : Int
itemsPerPage =
    30



-- ROUTES


routes : Parser (Route -> a) a
routes =
  UP.oneOf
    [ UP.map PageRoute (UP.s "page" </> UP.int)
    ]



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest request =
    UrlRequest request


onUrlChange : Url.Url -> Msg
onUrlChange url =
    UrlChange url


init : () ->  Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { key = key
            , url = url
            , title = "Hacker News"
            , timeZone = Time.utc
            , hackernews = HN.empty itemsPerPage
            , showComments = False
            }

        ( newModel, cmd ) =
            handleUrl url model
    in
    ( newModel, Cmd.batch [ cmd, Task.perform GotTimeZone Time.here ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ BE.onKeyDown (Decode.map KeyDown E.keyCode)
        ]


updateHackernews : Model -> ( HN.Model, Cmd HN.Msg ) -> ( Model, Cmd Msg )
updateHackernews newModel ( hackernews, cmd ) =
    ( { newModel | hackernews = hackernews }, Cmd.map HackerNewsMsg cmd )


handleUrl : Url.Url -> Model -> ( Model, Cmd Msg )
handleUrl url model =
    let
        maybeRoute =
            UP.parse routes url

        ( newModel, cmd ) =
            case maybeRoute of
                Just route ->
                    case route of
                        PageRoute page ->
                            -- todo: check page range
                            model.hackernews |> HN.setPage (page - 1) |> updateHackernews model

                Nothing ->
                    ( model, Cmd.none )
    in
    ( { newModel | url = url }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setShowComments m =
            { m | showComments = True }
    in
    case msg of
        -- fetch top stories after retrieving time zone
        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.map HackerNewsMsg HN.fetchTopStories )

        HackerNewsMsg submsg ->
            model.hackernews |> HN.update submsg |> updateHackernews model

        UpdatePage page ->
            model.hackernews |> HN.setPage page |> updateHackernews model

        ShowComments itemId ->
            model.hackernews
                |> HN.fetchChildren itemId
                |> updateHackernews model
                |> Tuple.mapFirst setShowComments

        HideComments ->
            ( { model | showComments = False }, Cmd.none )

        KeyDown keyCode ->
            let
                newModel =
                    if keyCode == 27 then
                        { model | showComments = False }

                    else
                        model
            in
            ( newModel, Cmd.none )

        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChange url ->
            handleUrl url model


-- VIEW


{-| Conditionally render node
-}
nodeIf : String -> List (H.Attribute msg) -> List (H.Html msg) -> Bool -> H.Html msg
nodeIf name attrs children flag =
    if flag then
        H.node name attrs children

    else
        H.text ""


{-| Make keyed item.
-}
keyedItem : HN.Item -> ( String, H.Html Msg )
keyedItem item =
    ( String.fromInt (HN.itemId item), Lazy.lazy viewPost item )


{-| View.
-}
view : Model -> Browser.Document Msg
view model =
    let
        items =
            HN.currentItems model.hackernews

        itemStart =
            model.hackernews.page * itemsPerPage + 1

        pagesCount =
            HN.pagesCount model.hackernews

        comment =
            HN.currentComments model.hackernews
                |> MT.children
    in
    { title = model.title
    , body =
        [ H.node "link" [ A.rel "stylesheet", A.href "base.css" ] []
        , H.div [ A.class "page-wrapper" ]
            [ H.h1 [ A.class "page-title" ]
                [ H.a [ A.href HN.pageUrl, A.rel "noreferrer" ] [ H.text model.title ]
                ]
            , Lazy.lazy viewError model.hackernews.error
            , Lazy.lazy2 viewItems items itemStart
            , Lazy.lazy2 viewPaging pagesCount model.hackernews.page
            , nodeIf "div"
                [ A.class "comments-wrapper" ]
                [ H.div [ A.class "comments-content" ]
                    [ Lazy.lazy viewComments comment
                    ]
                , H.button [ A.class "comments-close", E.onClick HideComments ] [ H.text "Close" ]
                ]
                model.showComments
            ]
        ]
    }


{-| View error.
-}
viewError : Maybe String -> H.Html Msg
viewError error =
    let
        text =
            error |> Maybe.withDefault ""
    in
    nodeIf "div"
        [ A.class "error-message" ]
        [ H.text text ]
        (error /= Nothing)


{-| Post items.
-}
viewItems : List HN.Item -> Int -> H.Html Msg
viewItems items start =
    Keyed.node "ol"
        [ A.class "post-list"
        , A.start start
        ]
        (List.map keyedItem items)


{-| Paging.
-}
viewPaging : Int -> Int -> H.Html Msg
viewPaging count page =
    let
        listItem n =
            let
                href =
                    "/page/" ++ String.fromInt (n + 1)
            in
            H.li
                [ A.class "paging__page"
                , A.classList [ ( "paging--active", page == n ) ]
                --, E.onClick (UpdatePage n)
                ]
                [ H.a [ A.href href ] [ H.text (String.fromInt (n + 1)) ]
                ]

        pages =
            ListExtra.initialize count listItem
    in
    H.ul [ A.class "paging" ] pages


{-| Comments.
-}
viewComments : List (MT.Tree HN.Item) -> H.Html Msg
viewComments children =
    let
        post i n =
            H.li []
                [ viewPost i
                , if List.length n > 0 then
                    viewComments n

                else
                    H.text ""
                ]

        keyedTree (MT.Tree child subchildren) =
            ( String.fromInt <| HN.itemId child, post child subchildren )

        posts =
            List.map keyedTree children
    in
    Keyed.node "ul" [ A.class "comments-list" ] posts


{-| Post.
-}
viewPost : HN.Item -> H.Html Msg
viewPost post =
    let
        markdownOptions =
            let
                options =
                    MarkdownConfig.defaultOptions
            in
            { options | rawHtml = MarkdownConfig.ParseUnsafe }
    in
    case post of
        HN.ItemPlaceholder id ->
            viewPostTitle
                { class = "post--placeholder"
                , title = "\u{00A0}"
                , info = [ H.text "\u{00A0}" ]
                , href = Nothing
                }

        HN.ItemStory story ->
            let
                text =
                    String.fromInt story.score ++ " points by " ++ story.by ++ " | "

                comments =
                    String.fromInt story.descendants ++ " comments"

                info =
                    [ H.text text
                    , H.a [ A.href "#comments", E.onClick (ShowComments story.id) ] [ H.text comments ]
                    ]
            in
            viewPostTitle
                { class = ""
                , title = story.title
                , info = info
                , href = story.url
                }

        HN.ItemComment comment ->
            let
                info =
                    [ H.text ("by " ++ comment.by ++ " | ") ]

                text =
                    "<div>" ++ comment.text ++ "</div>"

                header =
                    H.span [ A.class "post-info" ]
                        [ H.text comment.by
                        ]

                markdown =
                    Markdown.toHtml (Just markdownOptions) text
            in
            H.div [ A.class "comment" ] (header :: markdown)

        HN.ItemJob job ->
            let
                infoText =
                    String.fromInt job.score ++ " points by " ++ job.by
            in
            viewPostTitle
                { class = ""
                , title = job.title
                , info = [ H.text infoText ]
                , href = job.url
                }


{-| Post title.
-}
viewPostTitle :
    { class : String
    , title : String
    , info : List (H.Html Msg)
    , href : Maybe String
    }
    -> H.Html Msg
viewPostTitle { class, title, info, href } =
    let
        textNode =
            H.text title

        link url =
            H.a [ A.href url, A.rel "noreferrer" ] [ textNode ]

        titleNode =
            href
                |> Maybe.map link
                |> Maybe.withDefault textNode
    in
    H.li [ A.class "post", A.class class ]
        [ H.span [ A.class "post-title" ] [ titleNode ]
        , H.span [ A.class "post-info" ] info
        ]
