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
    , error : Maybe String
    , timeZone : Time.Zone
    , items : List Item
    , itemIds : List Int
    , itemsCache : Dict.Dict Int HN.Item
    , page : Int
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
            , error = Nothing
            , timeZone = Time.utc
            , items = []
            , itemIds = []
            , itemsCache = Dict.empty
            , page = 0
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


pagesCount : Model -> Int
pagesCount model =
    (List.length model.itemIds + itemsPerPage - 1) // itemsPerPage


replacePlaceholderItem : Int -> Item -> List Item -> List Item
replacePlaceholderItem itemId newItem =
    List.map (\item ->
        case item of
            ItemPlaceholder id ->
                if id == itemId then newItem else item

            _ -> item
    )


paging : Int -> List a -> List a
paging page =
    List.drop (itemsPerPage * page)
        >> List.take itemsPerPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- fetch top stories after retrieving time zone
        GotTimeZone timeZone ->
            ( { model | timeZone = timeZone }, Cmd.map HackerNewsMsg HN.fetchTopStories )

        HackerNewsMsg submsg ->
            case submsg of
                HN.GotTopStories result ->
                    case result of
                        Ok list ->
                            let
                                newModel = 
                                    { model
                                        | itemIds = list
                                    }   
                            in
                            update (UpdatePage model.page) newModel

                        Err err ->
                            ( { model
                                | error = Just (resultErrorString err)
                            }, Cmd.none )

                HN.GotItem id result ->
                    case result of
                        Ok item ->
                            ( { model
                                | items = replacePlaceholderItem id (Item item) model.items
                                , itemsCache = Dict.insert id item model.itemsCache
                            }, Cmd.none )

                        Err err ->
                            ( { model
                                | items = replacePlaceholderItem id (ItemError id (resultErrorString err)) model.items
                            }, Cmd.none )

        UpdatePage page ->
            let
                getItem id =
                    case Dict.get id model.itemsCache of
                        Just item ->
                            Item item

                        Nothing ->
                            ItemPlaceholder id

                itemList =
                    model.itemIds
                        |> paging page
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
                        |> List.map getItemId

                newModel =
                    { model
                        | page = page
                        , items = itemList
                    }
            in
            ( newModel, Cmd.map HackerNewsMsg (HN.fetchItems loadItems) )


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
            ( String.fromInt (getItemId item), Lazy.lazy viewPost item )

        page n =
            H.li
                [ A.class "paging__page"
                , A.classList [("paging--active", model.page == n)]
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
                [ H.text (model.error |> Maybe.withDefault "") ]
                (model.error /= Nothing)
            , Keyed.node "ol" [ A.class "post-list", A.start (model.page * itemsPerPage + 1) ]
                (List.map itemKeyed model.items)
            , H.ul [ A.class "paging" ]
                (for page <| pagesCount model)
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
