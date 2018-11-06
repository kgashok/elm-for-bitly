module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (field, string, list, decodeString, Decoder)

testJson : String
testJson =
    "https://api.myjson.com/bins/19yily"  -- nicknames
    --"https://api.myjson.com/bins/skw8e"  -- 100 links from bitly
    --"https://api.myjson.com/bins/wz9me"
    
{--
linkDecoder : Decoder Link
linkDecoder =
    Json.Decode.object2
        User
        ("title" := Decode.string)
        ("long_url" := Decode.string)
 -} 

nicknamesDecoder : Decoder (List String)
nicknamesDecoder = 
  field "nicknames" (list string) 


type alias Link = 
  { title : String
  -- , keyworld_link: Maybe String
  , long_url : String 
  }

  

httpCommand : Cmd Msg
httpCommand =
    nicknamesDecoder
        |> Http.get testJson
        |> Http.send DataReceived
        

type Match
    = Yes
    | No
    | NA -- needle is empty and therefore 'not applicable'


-- Boolean blindness in Elm
-- https://discourse.elm-lang.org/t/fixing-boolean-blindness-in-elm/776

type alias HayString =
    { hay : String
    , match : Match  -- why not Bool? Because Elm is Boolean Blind?
    }


type alias Model =
    { val : Int
    , needle : String
    , hay : List HayString
    , errorMessage : Maybe String
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( { val = 0
    , needle = "rawgit"
    , hay =
        [ HayString "http://rawgit.com zee" Yes
        , HayString "http://google.com" No
        , HayString "http://junk.com tez" No
        , HayString "http://abcde.org" No
        ]
    , errorMessage = Nothing
    }, Cmd.none)


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
    | DataReceived (Result Http.Error (List String))
    

--update : Msg -> Model -> Model
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest -> 
            ( model, httpCommand )
            
        Increment ->
            ({ model | val = model.val + 1 }, Cmd.none)

        Decrement ->
            ({ model | val = model.val - 1 }, Cmd.none)

        StoreNeedle s ->
            ({ model | needle = s, hay = checkForMatches s model.hay }, Cmd.none)

        StoreHay h ->
            -- {model|hay = [h], match = checkForMatch model.needle h}
            (model, Cmd.none)
        
        DataReceived (Ok nicknames) ->
              -- ( { model | hay = nicknames }, Cmd.none)
            ( {model 
                | hay = makeHayFromNames model.needle nicknames
                , errorMessage = Nothing 
              }
            , Cmd.none)
            
        DataReceived (Err httpError) ->      
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
              }
            , Cmd.none
            )
            
makeHayFromNames needle names = 
  names
    |> List.map (\x -> HayString x NA) 
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

        -- , buttonDisplay model
        , div []
            [ text "Needle "
            , input [ placeholder model.needle, onInput StoreNeedle ] []
            ]
        , div []
            [ text "Hay (a list of URLs strings stored in bitly)"
            , generateListView model.hay
            ]
        , hr [] []
        , button [ onClick SendHttpRequest ] [ text "Fetch URLs" ]
        , div [ id "error"] [ text (Maybe.withDefault "status: Ok" model.errorMessage) ]
        , footer
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


checkForMatch : String -> HayString -> HayString
checkForMatch needle hays =
    case not (String.isEmpty needle) of
        True ->
            let
                needle_ =
                    needle
                        |> String.trim
                        |> String.toLower
                        
                hay_ = String.toLower hays.hay
                
            in
            case String.contains needle_ hay_ of
                True ->
                    HayString hays.hay Yes

                _ ->
                    HayString hays.hay No

        False ->
            HayString hays.hay NA


checkForMatches : String -> List HayString -> List HayString
checkForMatches needle haylist =
    haylist
        |> List.map (checkForMatch needle)


matchString : Match -> String
matchString m =
    case m of
        Yes ->
            " Yes! "

        No ->
            " No "

        NA ->
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

