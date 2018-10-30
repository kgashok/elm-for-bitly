module Main exposing (Msg(..), main, update, view)

import Browser

{-
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (placeholder, css, href, src)
import Html.Styled.Events exposing (onClick, onInput)
-}

--import Html exposing (Html, a, button, div, img, text, input, hr, br, span)
--import Html.Attributes exposing (alt, href, src, placeholder, value)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

-- Boolean blindness in Elm 
-- https://discourse.elm-lang.org/t/fixing-boolean-blindness-in-elm/776
type alias Model = 
  { val: Int
  , needle: String
  , hay: List String
  , match: String 
  }



-- from https://elm-lang.org/docs/syntax#comments
{-}
add x y = x + y
--}

init : Model
init = 
  { val = 0
  , needle = "NEEDLE"
  , hay = [ "Hay containsNEEDLE in it!"
          , "Hay has no NEEDLE"
          , "Another one"
          ]
  , match = "No"}

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
            {model|needle = s, match = checkForMatch s (getFirst model.hay)}
        
        StoreHay h -> 
            {model|hay = [h], match = checkForMatch model.needle h}
            

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
                 --, input [ placeholder (getFirst model.hay), onInput StoreHay ][]
                 , generateListView model.hay
                 ]
        , hr [] []
        , text model.match
        , footer
        ]



gitRepo = "https://github.com/kgashok/elm-for-bitly"


footer : Html Msg
footer =
    div [ id "footer" ]
        [ a
            [ href (gitRepo ++ "/issues/new")
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text "No Versioning" ]
        ]


generateListView: List String -> Html Msg 
generateListView slist = 
  let 
    items = List.map viewInput slist 
  in 
    div [] [ ul [] items] 
  
  
viewInput p =
      input [ placeholder p, onInput StoreHay ] [] 
    

--viewInput : String -> String -> String -> (String -> msg) -> Html msg
--viewInput t p v toMsg =
  --input [ type_ t, placeholder p, value v, onInput toMsg ] []

checkForMatch: String -> String -> String 
checkForMatch needle hay = 
  case String.contains needle hay of 
    True -> "Yes!"
    False -> "No"
    
    
getFirst: List String -> String
getFirst slist = 
  Maybe.withDefault "NA" (List.head slist)
