module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- Boolean blindness in Elm
-- https://discourse.elm-lang.org/t/fixing-boolean-blindness-in-elm/776


type Match
    = Yes
    | No
    | NA


matchString : Match -> String
matchString m =
    case m of
        Yes ->
            " Yes! "

        No ->
            " No "

        NA ->
            " - "


type alias HayString =
    { hay : String
    , match : Match
    }


type alias Model =
    { val : Int
    , needle : String
    , hay : List HayString
    }


init : Model
init =
    { val = 0
    , needle = "rawgit"
    , hay =
        [ HayString "http://rawgit.com" Yes
        , HayString "http://google.com" No
        , HayString "http://junk.com" No
        ]
    }


main =
    Browser.sandbox { init = init, view = view, update = update }


type Msg
    = Increment
    | Decrement
    | StoreNeedle String
    | StoreHay String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | val = model.val + 1 }

        Decrement ->
            { model | val = model.val - 1 }

        StoreNeedle s ->
            { model | needle = s, hay = checkForMatches s model.hay }

        StoreHay h ->
            -- {model|hay = [h], match = checkForMatch model.needle h}
            model


view : Model -> Html Msg
view model =
    div []
        [ div [ id "title" ] [ text "Elm App in Glitch" ]

        -- , buttonDisplay model
        , hr [] []
        , div []
            [ text "Needle "
            , input [ placeholder model.needle, onInput StoreNeedle ] []
            ]
        , div []
            [ text "Hay (a list of URLs strings stored in bitly)"

            --, input [ placeholder (getFirst model.hay), onInput StoreHay ][]
            , generateListView model.hay
            ]
        , hr [] []
        , footer
        ]


buttonDisplay : Model -> Html Msg
buttonDisplay model =
    div []
        [ div [] [ text "Counter" ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.val) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


gitRepo =
    "https://github.com/kgashok/elm-for-bitly"


footer : Html Msg
footer =
    div [ id "footer" ]
        [ a
            [ href (gitRepo ++ "/issues/new")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text "Provide feedback?" ]
        ]


generateListView : List HayString -> Html Msg
generateListView slist =
    let
        items =
            List.map viewInput slist
    in
    div [] [ ul [] items ]


viewInput hs =
    div []
        [ input [ hayBackGround hs.match, placeholder hs.hay, onInput StoreHay ] []
        , text (matchString hs.match)
        ]


hayBackGround : Match -> Attribute msg
hayBackGround val =
    classList [ ( "matched", val == Yes ) ]



--viewInput : String -> String -> String -> (String -> msg) -> Html msg
--viewInput t p v toMsg =
--input [ type_ t, placeholder p, value v, onInput toMsg ] []


checkForMatch : String -> HayString -> HayString
checkForMatch needle hays =
    let
        needle_ =
            needle
                |> String.trim
                |> String.toLower
    in
    let
        emptyNeedle =
            String.isEmpty needle_
    in
    case emptyNeedle of
        True ->
            HayString hays.hay NA

        _ ->
            case String.contains needle_ hays.hay of
                True ->
                    HayString hays.hay Yes

                _ ->
                    HayString hays.hay No


checkForMatches : String -> List HayString -> List HayString
checkForMatches needle haylist =
    haylist
        |> List.map (checkForMatch needle)


getFirst : List String -> String
getFirst slist =
    Maybe.withDefault "NA" (List.head slist)



-- from https://elm-lang.org/docs/syntax#comments
-- Remove/add the } below and toggle between commented and uncommented


{--}
add x y =
    x + y
--}
