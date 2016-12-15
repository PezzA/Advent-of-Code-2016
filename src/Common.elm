module Common exposing (..)

import Array exposing (get)
import String


stringToInt : String -> Int
stringToInt input =
    case String.toInt input of
        Ok val ->
            val

        Err err ->
            -1


arrayBitToInt index array =
    case Array.get index array of
        Just arraybit ->
            case arraybit of
                Just stringval ->
                    case String.toInt stringval of
                        Ok parsedVal ->
                            parsedVal

                        Err err ->
                            -1

                Nothing ->
                    -1

        Nothing ->
            -1


renderPixel : Bool -> String
renderPixel value =
    case value of
        False ->
            "."

        True ->
            "#"
