module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy2)
import Http
import Json.Decode exposing (Decoder, decodeString, field, list, map2, maybe, string)
import Keyboard exposing (RawKey)
import Process
import Task


{-| apiKey needs to be hidden but it is okay for now
-}
apiKey =
    "1ef1315a2efebd7557de137f776602276d833cb9"


bitlyAPI =
    "https://api-ssl.bitly.com/v3/user/link_history?access_token=" ++ apiKey


{-| pagesize defines the number of links to be retrieved in
in one http request
-}
pagesize =
    String.fromInt 100


{-| test data to check out the logic before actually going out to
the bitly API. Quick response also ensure quicker iterations of debugging bugs
-}
testJson : String
testJson =
    "https://api.myjson.com/bins/skw8e"


{-| Link is the basic data type that is used to implement the logic
for this program. Link is converted to HayString for some reason.
I used HayString during the testing phase, and will need to refactor it.
Alternatively, by leaving that in, we can convert some other type
and search on that as well. Something to ponder...
-}
type alias Link =
    { title : String
    , keyword_link : Maybe String
    , long_url : String
    , tags : List String
    }


{-| Match is used instead of Bool True or False. Why?
Two reasons

  - something to do with the language
    -- Boolean blindness in Elm
    -- <https://discourse.elm-lang.org/t/fixing-boolean-blindness-in-elm/776>
  - and probably better readability?

-}
type Match
    = Yes
    | No


type MatchMode
    = AllNeedles
    | AnyOneNeedle


{-| HayString resonates with the basic problem that this app is trying to solve
and that is to represent data that needs to be searched and whether
the searched 'needle' was found at all
-}
type alias HayString =
    { hay : String
    , title : String
    , short : Maybe String -- not every link has been customized to be easily recalled
    , tags : List String
    , dump : String -- dump of everything in the above fields
    , match : Maybe Match -- why not Bool? See documentation for Match type
    }


{-| Are we using test data or actual data from Bitly?
-- Based on the data source, sometimes we have to use different
-- decoders to get the data into Elm variables in the Model
-}
type DataSource
    = SimpleList
    | Test
    | Production


{-| ViewMode determines whether you want to show all searched haystrings or
only those that matched?
-}
type ViewMode
    = ShowAll
    | ShowMatched
    | ShowAny


{-| Model is what captures what all that is required to make this app work
It uses data types that have been defined earlier in this module above
-}
type alias Model =
    { val : Int -- not relevant
    , needle : String
    , hay : List HayString
    , errorMessage : Maybe String
    , errorStatus : Bool
    , dataAPI : String
    , data : DataSource
    , viewMode : ViewMode
    , linkcount : Int
    , offset : Int -- required for obtaining pages of information from API
    , pressedKeys : List Keyboard.Key
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { val = 0
      , needle = "com junk"
      , hay =
            [ HayString "http://rawgit.com" "" Nothing [] "http://rawgit.com" Nothing
            , HayString "http://google.com" "" Nothing [ "search" ] "http://google.com" Nothing
            , HayString "http://junk.com" "" Nothing [ "junk", "archive" ] "http://junk.com" Nothing
            , HayString "http://abcde.org" "" Nothing [] "http://abcde.org" Nothing
            ]
      , errorMessage = Nothing
      , errorStatus = False
      , dataAPI = bitlyAPI
      , data = Production
      , viewMode = ShowAll
      , linkcount = 1700
      , offset = 0
      , pressedKeys = []
      }
        |> (\model -> { model | hay = checkForMatches model.needle model.hay })
      -- , Task.perform (always StoreNeedle "rawgit")
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
        , subscriptions = subscriptions
        }



{-
   subscriptions : Model -> Sub Msg
   subscriptions model =
       Sub.batch
           [ Keyboard.downs KeyDown

           -- , Keyboard.ups KeyUp
           -- , windowBlurs ClearKeys
           ]
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.subscriptions
        , Keyboard.downs KeyDown

        -- , Keyboard.ups KeyUp
        ]


type Msg
    = StoreNeedle String
    | SearchNeedle
    | SwitchTo DataSource
    | ChangeViewTo ViewMode
    | SendHttpRequest
    | DataReceived (Result Http.Error (List Link))
    | DataSReceived (Result Http.Error (List (List Link)))
    | IncDataReceived (Result Http.Error (List Link))
    | NamesReceived (Result Http.Error (List String))
    | UpdateLinkCount String
    | KeyDown RawKey
    | KeyboardMsg Keyboard.Msg
    | Increment -- not relevant; legacy
    | Decrement -- not relevant; legacy


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
                            "deep docs"

                        _ ->
                            "medium python time"

                model_ =
                    { model
                        | needle = needle_
                        , hay = []
                        , viewMode = ShowMatched
                        , errorMessage = Just "Launching requests..."
                        , offset = 0
                    }

                dataRequestTask =
                    case model_.linkcount > 1700 of
                        True ->
                            -- bitlySeqRequest model_.dataAPI model_.linkcount
                            bitlyIncRequest model_.dataAPI model_.linkcount model_.offset

                        False ->
                            Cmd.batch (bitlyBatchRequest model_.dataAPI model_.linkcount)
            in
            case model_.data of
                Production ->
                    ( model_, dataRequestTask )

                _ ->
                    ( model_, httpCommand model.dataAPI )

        StoreNeedle s ->
            ( { model
                | needle = s

                -- , hay = checkForMatches s model.hay
              }
            , Cmd.none
            )

        SearchNeedle ->
            ( { model
                | hay = checkForMatches model.needle model.hay
              }
            , Cmd.none
            )

        NamesReceived (Ok nicknames) ->
            ( { model
                | hay = makeHayFromNames model.needle nicknames
                , errorMessage = Nothing
                , errorStatus = False
              }
            , Cmd.none
            )

        NamesReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
                , errorStatus = True
              }
            , Cmd.none
            )

        IncDataReceived (Ok urls) ->
            let
                updatedHays =
                    model.hay ++ makeHayFromUrls model.needle urls

                incOffset =
                    case model.offset < model.linkcount of
                        True ->
                            model.offset + 100

                        False ->
                            0

                nextCmd =
                    case incOffset < model.linkcount of
                        True ->
                            bitlyIncRequest model.dataAPI model.linkcount incOffset

                        False ->
                            Cmd.none
            in
            ( { model
                | hay = updatedHays
                , errorMessage =
                    (++) (Maybe.withDefault "" model.errorMessage) " ."
                        |> flip (++) (String.fromInt model.offset)
                        -- |> (\message -> (++) message (String.fromInt model.offset))
                        |> Just
                , errorStatus = False
                , offset = incOffset
              }
            , nextCmd
            )

        IncDataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
                , errorStatus = True
              }
            , Cmd.none
            )

        DataReceived (Ok urls) ->
            let
                previous =
                    model.hay
            in
            ( { model
                | hay = makeHayFromUrls model.needle urls ++ previous
                , errorMessage = Nothing
                , errorStatus = False
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
                , errorStatus = True
              }
            , Cmd.none
            )

        DataSReceived (Ok listOfListUrls) ->
            let
                urllist =
                    listOfListUrls |> List.concat
            in
            ( { model
                | hay = makeHayFromUrls model.needle urllist
                , errorMessage = Nothing
                , errorStatus = False
              }
            , Cmd.none
            )

        DataSReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (createErrorMessage httpError)
                , errorStatus = True
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

        ChangeViewTo v ->
            ( { model
                | viewMode = v
              }
            , Cmd.none
            )

        UpdateLinkCount c ->
            ( { model
                | linkcount = Maybe.withDefault 10 (String.toInt c)
              }
            , Cmd.none
            )

        KeyboardMsg keyboardMsg ->
            let
                model_ =
                    { model | pressedKeys = Keyboard.update keyboardMsg model.pressedKeys }
            in
            case
                List.member Keyboard.Control model_.pressedKeys
                    && List.member (Keyboard.Character "q") model_.pressedKeys
            of
                True ->
                    case model_.viewMode of
                        ShowAll ->
                            ( { model_
                                | viewMode = ShowMatched
                                , errorMessage = Just "Press Ctrl-q to toggle view"
                              }
                            , Cmd.none
                            )
                        
                        ShowMatched -> 
                            ( { model_
                                | viewMode = ShowAny
                                , errorMessage = Just "Press Ctrl-q to toggle view"
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model_ | viewMode = ShowAll }, Cmd.none )

                False ->
                    ( model_, Cmd.none )

        KeyDown code ->
            {--
            let
                _ =
                    Debug.log "key code: " code
            in
            --}
            case Keyboard.characterKey code of
                Just (Keyboard.Character " ") ->
                    ( { model | hay = checkForMatches model.needle model.hay }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- irrelevant message types, to be removed eventually
        Increment ->
            ( { model | val = model.val + 1 }, Cmd.none )

        Decrement ->
            ( { model | val = model.val - 1 }, Cmd.none )


{-| flip is now deprecated in Elm 0.19
-- Available here for practice
-}
flip func first second =
    func second first


bitlyIncRequest : String -> Int -> Int -> Cmd Msg
bitlyIncRequest dataURL count offset =
    let
        skipUrl url o =
            String.fromInt o
                |> (++) "&offset="
                |> (++) pagesize
                |> (++) "&limit="
                |> (++) url

        {--
        _ =
            Debug.log "url offset: " offset
        --}
    in
    urlsDecoder
        |> Http.get (skipUrl dataURL offset)
        |> Http.send IncDataReceived


httpCommand : String -> Cmd Msg
httpCommand dataURL =
    {--
    let
        _ =
            Debug.log "url: " dataURL
    in
    --}
    case dataURL of
        "https://api.myjson.com/bins/19yily" ->
            nicknamesDecoder
                |> Http.get dataURL
                |> Http.send NamesReceived

        _ ->
            urlsDecoder
                |> Http.get dataURL
                |> Http.send DataReceived


{-| skipList returns a list of numbers in intervals of 30.
-- this is required for parallel dispatch of ~30 requests
skipList 120 40
--> [0, 40, 80, 120]
skipList 170
--> [0, 30, 60, 90, 120, 150, 180]
-}
skipList : Int -> Maybe Int -> List Int
skipList totalCount pageSize =
    let
        size =
            Maybe.withDefault 30 pageSize

        rangeLimit =
            round (toFloat totalCount / toFloat size)
    in
    List.map (\x -> x * size) (List.range 0 rangeLimit)


{-| bitlyBatchRequest helps create a list of Http.gets
to get all the URLS for a specific user
-- uses skipList and skipUrl to generate a list
-- of Http requests
-}
bitlyBatchRequest : String -> Int -> List (Cmd Msg)
bitlyBatchRequest dataURL count =
    let
        skipUrl url offset =
            url ++ "&limit=" ++ pagesize ++ "&offset=" ++ String.fromInt offset
    in
    skipList count (String.toInt pagesize)
        |> List.map (skipUrl dataURL)
        |> List.map httpCommand


httpCommand2 dataURL =
    {--let
        _ =
            Debug.log "Sequential url: " dataURL
    in
    --}
    urlsDecoder
        |> Http.get dataURL
        |> Http.toTask


{--}
bitlySeqRequest dataURL count =
    let
        skipUrl url offset =
            url ++ "&limit=" ++ pagesize ++ "&offset=" ++ String.fromInt offset
    in
    skipList count (String.toInt pagesize)
        |> List.map (skipUrl dataURL)
        |> List.map httpCommand2
        |> List.map (\requestTask -> Task.andThen (always requestTask) (Process.sleep 300))
        |> Task.sequence
        |> Task.attempt DataSReceived
--}


parseKeyword : Maybe String -> Maybe String
parseKeyword short =
    let
        tlist =
            String.split "/" (Maybe.withDefault "" short)
    in
    List.head (List.reverse tlist)


checkForMatches : String -> List HayString -> List HayString
checkForMatches needle haylist =
    haylist
        -- |> List.map (\hs -> { hs | match = isMatch needle hs.dump })
        |> List.map (\hs -> { hs | match = listMatch AllNeedles needle hs.dump })


{-| isMatch is the crux of the whole app and is where all the
-- search action happens
-- If 'needle' is empty, then function returns 'Nothing'
-- If 'needle' is present and matched, the function returns a Match object 'Yes'
-- If 'needle' is present and not matched, the function returns a Match object 'No'
-- What if needle contains multiple words?
-- - can use String.split to create a list of words and
-- - Therefore, better to place the needle as the
-- - second argument?
-}
isMatch : String -> String -> Maybe Match
isMatch needle hay =
    {--let
        _ =
            Debug.log "needle and hay: " (needle ++ ":" ++ hay)
    in
    --}
    case not (String.isEmpty needle) of
        True ->
            let
                needle_ =
                    String.toLower <| String.trim needle

                hay_ =
                    String.toLower hay
            in
            case String.contains needle_ hay_ of
                True ->
                    Just Yes

                _ ->
                    Just No

        False ->
            Nothing


{-| listMatch checks for match of any or all of tokens in a list
-- Depending upon the MatchMode, the function will
-- return the appropriate value
listMatch AllNeedles "two points" "one two three main points"
--> True
listMatch AllNeedles "two points" "one two three..."
--> True
-}
listMatch : MatchMode -> String -> String -> Maybe Match
listMatch matchmode needle hay =
    let
        needlelist =
            String.split " " needle

        resultOf x accumulator =
            case ( x, accumulator ) of
                ( Just No, _ ) ->
                    Just No

                ( Just Yes, Just No ) ->
                    Just No

                ( Just Yes, _ ) ->
                    Just Yes

                ( _, Just Yes ) ->
                    Just Yes

                ( _, _ ) ->
                    Nothing
    in
    {- the map-reduce paradigm is adopted to
       - get the final value
    -}
    needlelist
        |> List.map (\token -> isMatch token hay)
        |> List.foldl resultOf Nothing


{-| makeHayFromUrls converts a List of Link object into a List of Haystring objects
-- The 'match' attribute is set if there is match with the needle
-- The dump is a concatenation of all strings from all fields
-}
makeHayFromUrls : String -> List Link -> List HayString
makeHayFromUrls needle urls =
    let
        makeHay link =
            link.long_url
                ++ link.title
                ++ Maybe.withDefault "" (parseKeyword link.keyword_link)
                ++ String.join " " link.tags

        linkToHay l =
            HayString l.long_url l.title l.keyword_link l.tags (makeHay l) Nothing
                -- |> (\hs -> { hs | match = isMatch needle hs.dump })
                |> (\hs -> { hs | match = listMatch AllNeedles needle hs.dump })
    in
    urls
        |> List.map linkToHay


makeHayFromNames needle names =
    names
        |> List.map (\x -> HayString x "" Nothing [] x Nothing)
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
        [ div [ id "title" ] [ text "Elm for Bitly" ]
        , footer
        , hr [] []
        , div [ id "apiString" ] [ text model.dataAPI ]
        , viewPicker
            [ ( "Nicknames", model.data == SimpleList, SwitchTo SimpleList )
            , ( "use Test data", model.data == Test, SwitchTo Test )
            , ( "Access bitly API", model.data == Production, SwitchTo Production )
            ]
        , button [ onClick SendHttpRequest ] [ text "Fetch URLs" ]
        , div []
            [ text " limited to "
            , input [ placeholder (String.fromInt model.linkcount), onInput UpdateLinkCount ] []
            ]
        , div [ id "error", classList [ ( "failed", model.errorStatus == True ) ] ]
            [ text (Maybe.withDefault "status: Ok" model.errorMessage) ]
        , hr [] []

        -- , buttonDisplay model
        , div []
            [ text "Needle "
            , input [ placeholder "search", value model.needle, onInput StoreNeedle ] []
            , button [ onClick SearchNeedle ] [ text "Search!" ]
            , text (" " ++ model.needle)
            ]
        , hr [] []
        , div []
            [ text "Hay (a list of URLs strings stored in bitly)"
            , viewPicker
                [ ( "Matched Only", model.viewMode == ShowMatched, ChangeViewTo ShowMatched )
                , ( "Show All", model.viewMode == ShowAll, ChangeViewTo ShowAll )
                ]
            , lazy2 generateListView model.viewMode model.hay
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


generateListView : ViewMode -> List HayString -> Html Msg
generateListView viewmode slist =
    let
        items =
            slist
                |> List.filter (\x -> viewmode == ShowAll || x.match == Just Yes)
                |> List.map displayURL
    in
    div [] [ ul [] items ]


{-| displayURL returns the HTML list item corresponding to a HayString
-- defines how the attributes of a haystring is to be displayed in the view
-}
displayURL : HayString -> Html msg
displayURL hs =
    let
        shortener =
            Maybe.withDefault "" hs.short

        tagString =
            if List.isEmpty hs.tags then
                ""

            else
                (++) "tags: " <| String.join ", " hs.tags
    in
    li [ classList [ ( "matched", hs.match == Just Yes ) ] ]
        [ div [] [ text hs.hay ]
        , div [ classList [ ( "hayTitle", True ) ] ] [ text hs.title ]
        , div [ classList [ ( "hayKey", True ) ] ]
            [ a
                [ href shortener
                , target "_blank"
                , rel "noopener noreferrer"
                ]
                [ text shortener ]
            ]
        , div [ classList [ ( "hayKey", True ) ] ]
            [ text tagString ]
        ]


{-| hayBackGround assigns the "matched" CSS attribute
-- if and only if the 'match' attribute is True
-}
hayBackGround : Maybe Match -> Attribute msg
hayBackGround val =
    case val of
        Just Yes ->
            classList [ ( "matched", True ) ]

        _ ->
            classList [ ( "matched", False ) ]



-- DECODERS for Json data accessed from various data sources


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
    Json.Decode.map4
        Link
        (field "title" string)
        (maybe (field "keyword_link" string))
        (field "long_url" string)
        (field "tags" (list string))



--  SCRATCH section for hacking other ideas


getFirst : List String -> String
getFirst slist =
    Maybe.withDefault "NA" (List.head slist)



-- from https://elm-lang.org/docs/syntax#comments
-- Remove/add the } below and toggle between commented and uncommented


{--}
add x y =
    x + y
--}
