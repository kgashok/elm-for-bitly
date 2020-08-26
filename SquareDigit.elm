module SquareDigit exposing (squareDigit)

import Maybe exposing (map, withDefault)
import String exposing (concat, fromChar, fromInt, toInt, toList)


toSquareString : Maybe Int -> Maybe String
toSquareString =
    map (\n -> n ^ 2) >> map fromInt


squareDigit : Int -> Int
squareDigit =
    fromInt
        -- make list of digital characters
        >> toList
        -- map each digit_char -> integer -> square string equivalent
        >> List.filterMap (fromChar >> toInt >> toSquareString)
        -- join all square strings
        >> concat
        -- convert back to number
        >> toInt
        >> withDefault 0