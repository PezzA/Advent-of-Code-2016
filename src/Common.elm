module Common exposing (..)

import String

type alias TestRecord =
    ( String, String )


stringToInt : String -> Int
stringToInt input =
    case String.toInt input of
        Ok val ->
            val

        Err err ->
            0