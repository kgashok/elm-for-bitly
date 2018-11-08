module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, map2, maybe, string)


apiKey =
    "1ef1315a2efebd7557de137f776602276d833cb9"


bitlyAPI =
    "https://api-ssl.bitly.com/v3/user/link_history?access_token=" ++ apiKey


testJson : String
testJson =
    "https://api.myjson.com/bins/skw8e"


nicknamesJson : String
nicknamesJson =
    "https://api.myjson.com/bins/19yily"


nicknamesDecoder : Decoder (List String)
nicknamesDecoder =
    field "nicknames" (list string)


urlsDecoder : Decoder (List Link)
urlsDecoder =
    Json.Decode.at [ "data", "link_history" ] (list linkDecoder)


linkDecoder : Decoder Link
linkDecoder =
    Json.Decode.map3
        Link
        (field "title" string)
        (maybe (field "keyword_link" string))
        (field "long_url" string)
--}


type alias Link =
    { title : String
    , keyword_link : Maybe String
    , long_url : String
    }


type Match
    = Yes
    | No



-- Boolean blindness in Elm
-- https://discourse.elm-lang.org/t/fixing-boolean-blindness-in-elm/776


type alias HayString =
    { hay : String
    , title : String
    , short : Maybe String
    , match : Maybe Match -- why not Bool? Because Elm is Boolean Blind?
    }


type DataSource
    = SimpleList
    | Test
    | Production


type alias Model =
    { val : Int
    , needle : String
    , hay : List HayString
    , errorMessage : Maybe String
    , dataAPI : String
    , data : DataSource
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { val = 0
      , needle = "rawgit"
      , hay =
            [ HayString "http://rawgit.com" "" Nothing (Just Yes)
            , HayString "http://google.com" "" Nothing (Just No)
            , HayString "http://junk.com" "" Nothing (Just No)
            , HayString "http://abcde.org" "" Nothing (Just No)
            ]
      , errorMessage = Nothing
      , dataAPI = testJson
      , data = Test
      }
    , Cmd.none
    )



{--
main =
    Browser.sandbox { init = init, view = view, update = update }
--}


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Increment
    | Decrement
    | StoreNeedle String
    | StoreHay String
    | SendHttpRequest
    | NamesReceived (Result Http.Error (List String))
    | DataReceived (Result Http.Error (List Link))
    | SwitchTo DataSource



--update : Msg -> Model -> Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            let
                needle_ =
                    case model.data of
                        SimpleList ->
                            "God"

                        Test ->
                            "color"

                        _ ->
                            "share"
            in
            ( { model | needle = needle_ }, httpCommand model.dataAPI )

        Increment ->
            ( { model | val = model.val + 1 }, Cmd.none )

        Decrement ->
            ( { model | val = model.val - 1 }, Cmd.none )

        StoreNeedle s ->
            ( { model | needle = s, hay = checkForMatches s model.hay }, Cmd.none )

        StoreHay h ->
            -- {model|hay = [h], match = checkForMatch model.needle h}
            ( model, Cmd.none )

        NamesReceived (Ok nicknames) ->
            ( { model
                | hay = makeHayFromNames model.needle nicknames
              }
            , Cmd.none
            )

        NamesReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
              }
            , Cmd.none
            )

        DataReceived (Ok urls) ->
            ( { model
                | hay = makeHayFromUrls model.needle urls
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
              }
            , Cmd.none
            )

        SwitchTo d ->
            ( { model
                | data = d
                , dataAPI =
                    case d of
                        SimpleList ->
                            nicknamesJson

                        Test ->
                            testJson

                        Production ->
                            bitlyAPI
              }
            , Cmd.none
            )


httpCommand : String -> Cmd Msg
httpCommand dataURL =
    case dataURL of
        "https://api.myjson.com/bins/19yily" ->
            nicknamesDecoder
                |> Http.get dataURL
                |> Http.send NamesReceived

        _ ->
            urlsDecoder
                |> Http.get dataURL
                |> Http.send DataReceived


makeHayFromUrls needle urls =
    urls
        |> List.map (\x -> HayString x.long_url x.title x.keyword_link Nothing)
        |> checkForMatches needle


makeHayFromNames needle names =
    names
        |> List.map (\x -> HayString x "" Nothing Nothing)
        |> checkForMatches needle


createErrorMessage : Http.Error -> String
createErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "It appears you don't have an Internet connection right now."

        Http.BadStatus response ->
            response.status.message

        Http.BadPayload message response ->
            message


view : Model -> Html Msg
view model =
    div []
        [ div [ id "title" ] [ text "Elm App in Glitch" ]
        , footer
        , hr [] []
        , div [ id "apiString" ] [ text model.dataAPI ]
        , viewPicker
            [ ( "Nicknames", model.data == SimpleList, SwitchTo SimpleList )
            , ( "use Test data", model.data == Test, SwitchTo Test )
            , ( "Access bitly API", model.data == Production, SwitchTo Production )
            ]
        , button [ onClick SendHttpRequest ] [ text "Fetch URLs" ]
        , div [ id "error" ] [ text (Maybe.withDefault "status: Ok" model.errorMessage) ]
        , hr [] []

        -- , buttonDisplay model
        , div []
            [ text "Needle "
            , input [ placeholder model.needle, onInput StoreNeedle ] []
            ]
        , hr [] []
        , div []
            [ text "Hay (a list of URLs strings stored in bitly)"
            , generateListView model.hay
            ]
        ]


viewPicker : List ( String, Bool, msg ) -> Html msg
viewPicker options =
    fieldset [] (List.map radio options)


radio : ( String, Bool, msg ) -> Html msg
radio ( name, isChecked, msg ) =
    label []
        [ input [ type_ "radio", checked isChecked, onClick msg ] []
        , text name
        ]


checkbox : msg -> String -> Html msg
checkbox msg name =
    label []
        [ input [ type_ "checkbox", onClick msg ] []
        , text name
        ]


buttonDisplay : Model -> Html Msg
buttonDisplay model =
    div []
        [ div [] [ text "Counter" ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.val) ]
        , button [ onClick Increment ] [ text "+" ]
        , hr [] []
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
            List.map displayURL slist
    in
    div [] [ ul [] items ]


displayURL hs =
    let
        shortener =
            Maybe.withDefault "" hs.short
    in
    li [ hayBackGround hs.match ]
        [ div [] [ text hs.hay ]
        , div [ classList [ ( "hayTitle", True ) ] ] [ text hs.title ]

        -- , div [ classList [ ( "hayKey", True ) ] ] [ text shortener ]
        , div [ classList [ ( "hayKey", True ) ] ]
            [ a
                [ href shortener
                , target "_blank"
                , rel "noopener noreferrer"
                ]
                [ text shortener ]
            ]
        ]


hayBackGround : Maybe Match -> Attribute msg
hayBackGround val =
    case val of
        Just Yes ->
            classList [ ( "matched", True ) ]

        _ ->
            classList [ ( "matched", False ) ]


checkForMatch : String -> HayString -> HayString
checkForMatch needle hays =
    case not (String.isEmpty needle) of
        True ->
            let
                needle_ =
                    needle
                        |> String.trim
                        |> String.toLower

                hay_ =
                    String.toLower hays.hay
            in
            case String.contains needle_ hay_ of
                True ->
                    { hays | match = Just Yes }

                _ ->
                    { hays | match = Just No }

        False ->
            { hays | match = Nothing }


checkForMatches : String -> List HayString -> List HayString
checkForMatches needle haylist =
    haylist
        |> List.map (checkForMatch needle)


matchString : Maybe Match -> String
matchString m =
    case m of
        Just Yes ->
            " Yes! "

        Just No ->
            " No "

        Nothing ->
            " - "


getFirst : List String -> String
getFirst slist =
    Maybe.withDefault "NA" (List.head slist)



-- from https://elm-lang.org/docs/syntax#comments
-- Remove/add the } below and toggle between commented and uncommented


{--}
add x y =
    x + y
--}
