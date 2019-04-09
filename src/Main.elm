module Main exposing (main)

import Browser
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Http
import Json.Decode as Decode
import Time
import Task
import HackerNews as HN

type alias Model =
    { title : String
    , error : Maybe String
    , timeZone : Time.Zone
    , posts : List Item
    }

type Item
    = ItemPlaceholder Int
    | ItemError Int String
    | Item HN.Item

type Msg
    = GotTimeZone Time.Zone
    | HackerNewsMsg HN.Msg


-- SETTINGS


postLimit : Int
postLimit =
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
            , error = Nothing
            , timeZone = Time.utc
            , posts = []
            }
    in
    ( model, Task.perform GotTimeZone Time.here )


resultErrorString : Http.Error -> String
resultErrorString error =
    case error of
        Http.BadStatus status -> "BadStatus: " ++ String.fromInt status
        Http.BadUrl url       -> "BadUrl: " ++ url
        Http.Timeout          -> "Timeout"
        Http.NetworkError     -> "NetworkError"
        Http.BadBody str      -> "BadBody: " ++ str

getItemId : Item -> Int
getItemId item =
    case item of
        ItemPlaceholder id -> id
        ItemError error _ -> 0
        Item value -> HN.itemId value

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.map HackerNewsMsg (HN.fetchTopStories postLimit) )

        HackerNewsMsg submsg ->
            let
                newModel =
                    case submsg of
                        HN.GotTopStories limit result ->
                            case result of
                                Ok list -> { model | posts = list |> List.take limit |> List.map (\id -> ItemPlaceholder id) }
                                Err err -> { model | error = Just (resultErrorString err) }

                        HN.GotItem id result ->
                            case result of
                                Ok item -> { model | posts = setItem (HN.itemId item) (Item item) model.posts }
                                Err err -> { model | posts = setItem id (ItemError id <| resultErrorString err) model.posts }

                setItem itemId newItem =
                    List.map (\i ->
                        case i of
                            ItemPlaceholder id ->
                                if id == itemId then newItem else i

                            _ -> i
                    )
            in
            ( newModel, Cmd.map HackerNewsMsg (HN.update submsg) )


-- VIEW


{-| Conditionally render node
-}
nodeIf : String -> List (H.Attribute msg) -> List (H.Html msg) -> Bool -> H.Html msg
nodeIf name attrs children flag =
    if flag then
        H.node name attrs children
    else
        H.text ""

fixInt : Int -> Int -> String
fixInt length i =
    let
        str =
            String.fromInt i
    in
    String.repeat (length - String.length str) "0" ++ str

formatDateTime : Time.Zone -> Time.Posix -> String
formatDateTime zone posix =
    let
        toMonth month =
            case month of
                Time.Jan -> 1
                Time.Feb -> 2
                Time.Mar -> 3
                Time.Apr -> 4
                Time.May -> 5
                Time.Jun -> 6
                Time.Jul -> 7
                Time.Aug -> 8
                Time.Sep -> 9
                Time.Oct -> 10
                Time.Nov -> 11
                Time.Dec -> 12
    in
    (String.fromInt <| Time.toYear zone posix) ++ "-" ++
        (fixInt 2 <| toMonth <| Time.toMonth zone posix) ++ "-" ++
        (fixInt 2 <| Time.toDay zone posix) ++ " " ++
        (fixInt 2 <| Time.toHour zone posix) ++ ":" ++
        (fixInt 2 <| Time.toMinute zone posix) ++ ":" ++
        (fixInt 2 <| Time.toSecond zone posix)


view : Model -> Browser.Document Msg
view model =
    let
        postKeyed item =
            ( String.fromInt <| getItemId item, Lazy.lazy viewPost item )
    in
    { title = model.title
    , body =
        [ H.node "link" [ A.rel "stylesheet", A.href "base.css" ] []
        , H.div [ A.class "page-wrapper" ]
            [ H.h1 [ A.class "page-title" ] [ H.text model.title ]
            , nodeIf "div" [ A.class "error-message" ]
                [ H.text (model.error |> Maybe.withDefault "") ]
                (model.error /= Nothing)
            , Keyed.node "ol" [ A.class "post-list" ]
                (List.map postKeyed model.posts)
            ]
        ]
    }

viewPost : Item -> H.Html Msg
viewPost post =
    case post of
        ItemPlaceholder id ->
            viewPostTitle "post--placeholder" (" ") (" ") Nothing

        ItemError id error ->
            viewPostTitle "post--error" error (" ") Nothing

        Item item ->
            case item of
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
