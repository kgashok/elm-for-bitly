module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, a, button, div, img, text, input, hr, br, span)
import Html.Attributes exposing (alt, href, src, placeholder, value)
import Html.Events exposing (onClick, onInput)

-- Boolean blindness in Elm 
-- https://discourse.elm-lang.org/t/fixing-boolean-blindness-in-elm/776
type alias Model = {val: Int, needle: String, hay: String, match: String }


-- from https://elm-lang.org/docs/syntax#comments
{-}
add x y = x + y
--}

init : Model
init =
  {val = 0, needle = "NEEDLE", hay = "Hay containsNEEDLE in it!", match = "No"}

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
            {model|val = model.val + 1}

        Decrement ->
            {model|val = model.val - 1}
        
        StoreNeedle s -> 
            {model|needle = s, match = checkForMatch s model.hay}
        
        StoreHay h -> 
            {model|hay = h, match = checkForMatch model.needle h}
            

view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Counter" ]
        , button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.val) ]
        , button [ onClick Increment ] [ text "+" ]
        , br [] []
        , div [] [ text "Needle " 
                 , input [ placeholder model.needle, onInput StoreNeedle ] []
                 ]
        , div [] [ text "Hay "
                 , input [ placeholder model.hay, onInput StoreHay ][]
                 ]
        , hr [] []
        , text model.match
        ]


checkForMatch: String -> String -> String 
checkForMatch needle hay = 
  case String.contains needle hay of 
    True -> "Yes!"
    False -> "No"
    
