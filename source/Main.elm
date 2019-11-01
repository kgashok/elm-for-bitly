module Main exposing (Msg(..), main, update, view)

import Browser
import DateFormat
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy4)
import Http
import Json.Decode exposing (Decoder, decodeString, field, int, list, map2, maybe, string)
import Keyboard exposing (RawKey)
import Process
import Task
import Time exposing (Posix, Zone, millisToPosix, toDay, toHour, toMinute, toMonth, toYear, utc)
import TimeZone



-- import Iso8601 exposing (fromTime)


{-| apiKey needs to be hidden but it is okay for now
-}
apiKey : String
apiKey =
    "1ef1315a2efebd7557de137f776602276d833cb9"


bitlyAPI : String
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
    , created_at : Int
    , modified_at : Int
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



-- using ViewMode for now to toggle between various modes
{--
type MatchMode
    = AllNeedles
    | AnyOneNeedle

--}


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
    , match : Maybe Bool -- why not Bool? See documentation for Match type
    , created : Int
    , modified : Int
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
    , darkMode : Bool
    , dateDisplay : Bool
    , sorted : Bool
    , zone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { val = 0
      , needle = "medium python"
      , hay =
            [ HayString "http://rawgit.com" "" Nothing [] "http://rawgit.com" Nothing 0 0
            , HayString "http://google.com" "" Nothing [ "search" ] "http://google.com" Nothing 0 0
            , HayString "http://junk.com" "" Nothing [ "junk", "archive" ] "http://junk.com" Nothing 0 0
            , HayString "http://abcde.org" "" Nothing [] "http://abcde.org" Nothing 0 0
            ]
      , errorMessage = Just "Getting latest 2000...->"
      , errorStatus = False
      , dataAPI = bitlyAPI
      , data = Production
      , viewMode = ShowMatched
      , linkcount = 5000
      , offset = 0
      , pressedKeys = []
      , darkMode = True
      , dateDisplay = True
      , sorted = False
      , zone = utc
      }
        |> (\model -> { model | hay = checkForMatches model.viewMode model.needle [] })
    , Cmd.batch ([ Task.perform AdjustTimeZone Time.here ] ++ bitlyBatchRequest bitlyAPI 2000)
      -- , bitlyIncRequest bitlyAPI 1701 0
      --, Task.perform (always FetchLatest)
      -- , Cmd.none
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
    | FetchLatest
    | DataReceived (Result Http.Error (List Link))
    | DataSReceived (Result Http.Error (List (List Link)))
    | IncDataReceived (Result Http.Error (List Link))
    | NamesReceived (Result Http.Error (List String))
    | UpdateLinkCount String
    | KeyDown RawKey
    | KeyboardMsg Keyboard.Msg
    | ToggleDarkMode
    | ToggleDateDisplay
    | SortLinks
    | AdjustTimeZone Time.Zone
    | Increment -- not relevant; legacy
    | Decrement -- not relevant; legacy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchLatest ->
            let
                model_ =
                    { model
                        | needle = ""
                        , data = Production
                        , dataAPI = bitlyAPI
                        , linkcount = 100
                        , viewMode = ShowAll
                        , offset = 0
                        , hay = []
                        , errorMessage = Just "Getting the latest 100 -> "
                    }
            in
            ( model_, bitlyIncRequest model_.dataAPI model_.linkcount model_.offset )

        SendHttpRequest ->
            let
                needle_ =
                    case model.data of
                        SimpleList ->
                            "God"

                        Test ->
                            "deep docs"

                        _ ->
                            "project_ideas"

                model_ =
                    { model
                        | needle = needle_
                        , hay = []
                        , viewMode = ShowMatched
                        , errorMessage = Just "Launching requests..."
                        , offset = 0
                    }

                dataRequestTask =
                    case model_.linkcount > 5000 of
                        True ->
                            -- bitlySeqRequest model_.dataAPI model_.linkcount
                            bitlyIncRequest model_.dataAPI model_.linkcount model_.offset

                        False ->
                            Cmd.batch (bitlyBatchRequest model_.dataAPI model_.linkcount)
            in
            case model_.data of
                Production ->
                    ( model_
                    , dataRequestTask
                    )

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
                | hay = checkForMatches model.viewMode model.needle model.hay
              }
            , Cmd.none
            )

        NamesReceived (Ok nicknames) ->
            ( { model
                | hay = makeHayFromNames model.viewMode model.needle nicknames
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
                    model.hay ++ makeHayFromUrls model.viewMode model.needle urls

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
                | hay = makeHayFromUrls model.viewMode model.needle urls ++ previous
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
                | hay = makeHayFromUrls model.viewMode model.needle urllist
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
            ( case v of
                ShowAll ->
                    { model
                        | viewMode = v
                    }

                _ ->
                    { model
                        | viewMode = v
                        , hay = checkForMatches v model.needle model.hay
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

                ctrlkey =
                    List.member Keyboard.Control model_.pressedKeys
            in
            case ctrlkey of
                False ->
                    ( model_, Cmd.none )

                _ ->
                    ( handleControlKeyShortCuts model_, Cmd.none )

        KeyDown code ->
            {--
            let
                _ =
                    Debug.log "key code: " code
            in
            --}
            case Keyboard.characterKey code of
                Just (Keyboard.Character " ") ->
                    ( { model | hay = checkForMatches model.viewMode model.needle model.hay }
                    , Cmd.none
                    )

                Just (Keyboard.Character "/") ->
                    ( { model
                        | hay = sortHay model.hay model.sorted
                        , sorted = not model.sorted
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ToggleDarkMode ->
            ( { model | darkMode = not model.darkMode }
            , Cmd.none
            )

        ToggleDateDisplay ->
            ( { model | dateDisplay = not model.dateDisplay }
            , Cmd.none
            )

        SortLinks ->
            ( { model
                | hay = sortHay model.hay model.sorted
                , sorted = not model.sorted
              }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        -- irrelevant message types, to be removed eventually
        Increment ->
            ( { model | val = model.val + 1 }, Cmd.none )

        Decrement ->
            ( { model | val = model.val - 1 }, Cmd.none )


handleControlKeyShortCuts : Model -> Model
handleControlKeyShortCuts model =
    let
        ctrlKeyOptions =
            [ "q", "c" ]

        keyPressed =
            ctrlKeyOptions
                |> List.map (\key -> List.member (Keyboard.Character key) model.pressedKeys)

        {-
           _ =
               Debug.log "keyPressed: " keyPressed
        -}
    in
    case keyPressed of
        [ True, _ ] ->
            case model.viewMode of
                ShowAll ->
                    { model
                        | viewMode = ShowMatched
                        , hay = checkForMatches ShowMatched model.needle model.hay
                        , errorMessage = Just "Press Ctrl-q to toggle view"
                    }

                ShowMatched ->
                    { model
                        | viewMode = ShowAny
                        , hay = checkForMatches ShowAny model.needle model.hay
                        , errorMessage = Just "Press Ctrl-q to toggle view"
                    }

                _ ->
                    { model | viewMode = ShowAll }

        [ _, True ] ->
            { model
                | dateDisplay = not model.dateDisplay
                , errorMessage = Just "Press Ctrl-c to toggle date display"
            }

        _ ->
            model


sortHay : List HayString -> Bool -> List HayString
sortHay haylist sorted =
    case sorted of
        False ->
            List.sortBy .created haylist

        _ ->
            List.reverse haylist


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


{-| skipList returns a list of numbers in intervals of 30 (default value).
-- this is required for parallel dispatch of ~30 requests
skipList 170
--> [0, 30, 60, 90, 120, 150, 180]
skipList 120 40
--> [0, 40, 80, 120]
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


checkForMatches : ViewMode -> String -> List HayString -> List HayString
checkForMatches viewmode needle haylist =
    haylist
        -- |> List.map (\hs -> { hs | match = isMatch needle hs.dump })
        |> List.map (\hs -> { hs | match = listMatch viewmode needle hs.dump })


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
-- Depending upon the ViewMode, the function will
-- return the appropriate value
listMatch ShowMatched "two points" "one two three main points" == Just True
listMatch ShowAny "two points" "one two three..." == Just True
-}
listMatch : ViewMode -> String -> String -> Maybe Bool
listMatch viewmode tokenstring text =
    let
        needlelist =
            String.split " " (String.trim <| tokenstring)

        boolIsMatch hay needle =
            case isMatch needle hay of
                Just Yes ->
                    True

                _ ->
                    False
    in
    case viewmode of
        ShowAny ->
            Just <| List.any (boolIsMatch text) needlelist

        ShowMatched ->
            Just <| List.all (boolIsMatch text) needlelist

        -- should never get called, actually!
        ShowAll ->
            Just <| List.all (boolIsMatch text) needlelist



-- Nothing


{-| makeHayFromUrls converts a List of Link object into a List of Haystring objects
-- The 'match' attribute is set if there is match with the needle
-- The dump is a concatenation of all strings from all fields
-}
makeHayFromUrls : ViewMode -> String -> List Link -> List HayString
makeHayFromUrls viewmode needle urls =
    let
        -- _ = Debug.log "viewMode needle urls" (viewmode, needle, urls)
        makeHay link =
            link.long_url
                ++ link.title
                ++ Maybe.withDefault "" (parseKeyword link.keyword_link)
                ++ String.join " " link.tags

        linkToHay l =
            HayString l.long_url l.title l.keyword_link l.tags (makeHay l) Nothing (l.created_at * 1000) (l.modified_at * 1000)
                -- |> (\hs -> { hs | match = isMatch needle hs.dump })
                |> (\hs -> { hs | match = listMatch viewmode needle hs.dump })
    in
    urls
        |> List.map linkToHay


makeHayFromNames viewmode needle names =
    names
        |> List.map (\x -> HayString x "" Nothing [] x Nothing 0 0)
        |> checkForMatches viewmode needle


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
    let
        themeButtonLabel =
            case model.darkMode of
                False ->
                    "dark"

                _ ->
                    "bright"
    in
    div [ classList [ ( "dark", model.darkMode == True ) ] ]
        [ div [ id "title" ] [ text "Bitly using Elm " ]
        , div [ id "darkButtonDiv" ]
            [ button [ id "darkButton", onClick ToggleDarkMode ] [ text themeButtonLabel ]
            , button [ id "sortButton", onClick SortLinks ] [ text "sort" ]
            ]
        , footer
        , hr [] []
        , div [ id "apiString" ] [ text model.dataAPI ]
        , viewPicker
            [ ( "Nicknames", model.data == SimpleList, SwitchTo SimpleList )
            , ( "use Test data", model.data == Test, SwitchTo Test )
            , ( "Access bitly API", model.data == Production, SwitchTo Production )
            ]
        , span []
            [ button [ onClick FetchLatest ] [ text "Fetch Latest" ]
            , button [ onClick SendHttpRequest ] [ text "Fetch URLs" ]
            , div []
                [ text " limited to "
                , input
                    [ placeholder (String.fromInt model.linkcount)
                    , value (String.fromInt model.linkcount)
                    , onInput UpdateLinkCount
                    ]
                    []
                ]
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
                [ ( "Match All", model.viewMode == ShowMatched, ChangeViewTo ShowMatched )
                , ( "Match Any", model.viewMode == ShowAny, ChangeViewTo ShowAny )
                , ( "Show All", model.viewMode == ShowAll, ChangeViewTo ShowAll )
                ]
            , lazy4 generateListView model.viewMode model.dateDisplay model.hay model.zone
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
            [ text "Provide feedback?;" ]
        , a
            [ href (gitRepo ++ "/commits/glitch")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text " last checkin" ]
        ]


{-| generateListView presents the HayString object
-- provided "ShowAll" is set or match attribute has been set
-}
generateListView : ViewMode -> Bool -> List HayString -> Time.Zone -> Html Msg
generateListView viewmode showdate haylist zone =
    let
        items =
            haylist
                |> List.filter (\x -> viewmode == ShowAll || x.match == Just True)
                |> List.map (displayURL showdate zone)
    in
    div [] [ ul [] items ]


{-| displayURL returns the HTML list item corresponding to a HayString
-- defines how the attributes of a haystring is to be displayed in the view
-}
displayURL : Bool -> Time.Zone -> HayString -> Html msg
displayURL showdate zone hs =
    let
        shortener =
            Maybe.withDefault "" hs.short

        title =
            if String.length hs.title == 0 then
                "<NA>"

            else
                hs.title

        tagString =
            if List.isEmpty hs.tags then
                ""

            else
                (++) "tags: " <| String.join ", " hs.tags

        hidekeyline =
            String.isEmpty shortener && String.isEmpty tagString

        dates =
            ourPrettyDate hs.created zone (Just "")
                ++ " "
                ++ ourPrettyDate hs.modified zone (Just "modified: ")
    in
    li [ classList [ ( "matched", hs.match == Just True ) ] ]
        [ div
            [ classList
                [ ( "created", True )
                , ( "displaydate", showdate == True )
                ]
            ]
            [ text dates

            -- text (String.fromInt hs.created)
            ]
        , div [ classList [ ( "hayTitle", True ) ] ]
            [ a
                [ href hs.hay
                , target "_blank"
                , rel "noopener noreferrer"
                ]
                [ text title ]
            ]
        , div
            [ classList
                [ ( "hayKey", True )
                , ( "hidekeyline", hidekeyline == True )
                ]
            ]
            [ a
                [ href shortener
                , target "_blank"
                , rel "noopener noreferrer"
                ]
                [ text shortener ]
            , div
                [ classList [ ( "hayKey", True ) ] ]
                [ text tagString ]
            ]
        ]



-- Let's create a custom formatter we can use later:


ourFormatter : Zone -> Posix -> String
ourFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]



-- With our formatter, we can format any date as a string!


ourTimezone : Zone
ourTimezone =
    utc



-- 2018-05-20T19:18:24.911Z


ourPosixTime : Int -> Posix
ourPosixTime ts =
    Time.millisToPosix ts



{--
Would make ourPrettyDate return:

"May 20th, 2018" : String


ourPrettyDate : Int -> Maybe String -> String
ourPrettyDate created_at label =
    -- ourFormatter ourTimezone ourPosixTime
    ourFormatter ourTimezone (ourPosixTime created_at)
--}


ourPrettyDate =
    displayDate


displayDate : Int -> Time.Zone -> Maybe String -> String
displayDate created_at zone label =
    let
        hourInfo =
            toHour zone (millisToPosix created_at)

        minuteInfo =
            toMinute zone (millisToPosix created_at)

        yearInfo =
            toYear zone (millisToPosix created_at)

        monthInfo =
            case toMonth zone (millisToPosix created_at) of
                Time.Jan ->
                    "Jan"

                Time.Feb ->
                    "Feb"

                Time.Mar ->
                    "Mar"

                Time.Apr ->
                    "Apr"

                Time.May ->
                    "May"

                Time.Jun ->
                    "Jun"

                Time.Jul ->
                    "Jul"

                Time.Aug ->
                    "Aug"

                Time.Sep ->
                    "Sep"

                Time.Oct ->
                    "Oct"

                Time.Nov ->
                    "Nov"

                Time.Dec ->
                    "Dec"

        dateInfo =
            toDay zone (millisToPosix created_at)
    in
    case created_at of
        0 ->
            " "

        _ ->
            Maybe.withDefault " " label
                ++ String.fromInt hourInfo
                ++ ":"
                ++ String.fromInt minuteInfo
                ++ ", "
                ++ monthInfo
                ++ "-"
                ++ String.fromInt dateInfo
                ++ " "
                ++ String.fromInt yearInfo


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
    field "nicknames" (Json.Decode.list string)


urlsDecoder : Decoder (List Link)
urlsDecoder =
    Json.Decode.at [ "data", "link_history" ] (Json.Decode.list linkDecoder)


linkDecoder : Decoder Link
linkDecoder =
    Json.Decode.map6
        Link
        (field "title" string)
        (maybe (field "keyword_link" string))
        (field "long_url" string)
        (field "tags" (Json.Decode.list string))
        (field "created_at" int)
        (field "modified_at" int)



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
