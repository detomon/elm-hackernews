module Main exposing (main)

import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Dict
import HackerNews as HN
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Http
import Json.Decode as Decode
import List.Extra as ListExtra
import Markdown
import Markdown.Config as MC
import MultiwayTree as MT
import Task
import Time
import Url exposing (Url)
import Url.Parser as UP exposing ((</>), Parser)


type alias Hackernews =
    HN.Model


type alias Model =
    { key : Nav.Key
    , url : Url
    , title : String
    , timeZone : Time.Zone
    , hackernews : Hackernews
    , showComments : Bool
    }


type Item
    = ItemPlaceholder Int
    | ItemError Int String
    | Item HN.Item


type Msg
    = UrlRequest Browser.UrlRequest
    | UrlChange Url
    | GotTimeZone Time.Zone
    | HackerNewsMsg HN.Msg
    | UpdatePage Int
    | ShowComments HN.ItemId
    | HideComments
    | KeyDown Int


type Route
    = PageHome
    | PageRoute Int


type KeyAction
    = KeyOther
    | KeyEscape



-- SETTINGS


itemsPerPage : Int
itemsPerPage =
    30



-- ROUTES


routes : Parser (Route -> a) a
routes =
    UP.oneOf
        [ UP.map PageHome UP.top
        , UP.map PageRoute (UP.s "page" </> UP.int)
        ]


pageUrl : Int -> String
pageUrl page =
    "/page/" ++ String.fromInt (page + 1)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
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
    ( newModel, Cmd.batch [ Task.perform GotTimeZone Time.here, cmd ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onKeyDown (Decode.map KeyDown E.keyCode)
        ]


updateHackernews : Model -> ( Hackernews, Cmd HN.Msg ) -> ( Model, Cmd Msg )
updateHackernews model ( hackernews, cmd ) =
    ( { model | hackernews = hackernews }, Cmd.map HackerNewsMsg cmd )


handleUrl : Url -> Model -> ( Model, Cmd Msg )
handleUrl url model =
    let
        handleRoute route =
            case route of
                PageHome ->
                    model.hackernews |> HN.setPage 0 |> updateHackernews model

                PageRoute page ->
                    -- todo: check page range
                    model.hackernews |> HN.setPage (page - 1) |> updateHackernews model

        ( newModel, cmd ) =
            Maybe.map handleRoute (UP.parse routes url)
                |> Maybe.withDefault ( model, Cmd.none )
    in
    ( { newModel | url = url }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        setShowComments show m =
            { m | showComments = show }
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
                |> Tuple.mapFirst (setShowComments True)

        HideComments ->
            ( setShowComments False model, Cmd.none )

        KeyDown keyCode ->
            let
                newModel =
                    case getKeyAction keyCode of
                        KeyEscape ->
                            setShowComments False model

                        _ ->
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
nodeIf : String -> List (H.Attribute msg) -> List (Html msg) -> Bool -> Html msg
nodeIf name attrs children flag =
    if flag then
        H.node name attrs children

    else
        H.text ""


{-| Make keyed item.
-}
keyedItem : HN.Item -> ( String, Html Msg )
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
        [ H.node "link" [ A.rel "stylesheet", A.href "/assets/base.css" ] []
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
viewError : Maybe String -> Html Msg
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
viewItems : List HN.Item -> Int -> Html Msg
viewItems items start =
    Keyed.node "ol"
        [ A.class "post-list"
        , A.start start
        ]
        (List.map keyedItem items)


{-| Paging.
-}
viewPaging : Int -> Int -> Html Msg
viewPaging count page =
    let
        listItem n =
            H.li
                [ A.class "paging__page"
                , A.classList [ ( "paging--active", page == n ) ]
                ]
                [ H.a [ A.href (pageUrl n) ] [ H.text (String.fromInt (n + 1)) ]
                ]

        pages =
            ListExtra.initialize count listItem
    in
    H.ul [ A.class "paging" ] pages


{-| Comments.
-}
viewComments : List (MT.Tree HN.Item) -> Html Msg
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
            ( String.fromInt <| HN.itemId child, Lazy.lazy2 post child subchildren )

        posts =
            List.map keyedTree children
    in
    Keyed.node "ul" [ A.class "comments-list" ] posts


markdownOptions : Maybe MC.Options
markdownOptions =
    let
        options =
            MC.defaultOptions
    in
    Just { options | rawHtml = MC.ParseUnsafe }


{-| Get key action from key code.
-}
getKeyAction : Int -> KeyAction
getKeyAction keyCode =
    case keyCode of
        27 ->
            KeyEscape

        _ ->
            KeyOther


{-| The non-breaking space.
-}
nobreakSpace : String
nobreakSpace =
    "\u{00A0}"


{-| Post.
-}
viewPost : HN.Item -> Html Msg
viewPost post =
    case post of
        HN.ItemPlaceholder id ->
            viewPostTitle
                { class = "post--placeholder"
                , title = nobreakSpace
                , info = [ H.text nobreakSpace ]
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
                    Markdown.toHtml markdownOptions text
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
    , info : List (Html Msg)
    , href : Maybe String
    }
    -> Html Msg
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
