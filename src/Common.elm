module Common exposing (..)

import String
import Html exposing (div, Html, text, code)
import List exposing (map)

type alias TestRecord =
    ( String, String )


stringToInt : String -> Int
stringToInt input =
    case String.toInt input of
        Ok val ->
            val

        Err err ->
            0
            
textToDiv : String -> Html msg
textToDiv textToDisplay = 
    div [] 
        [ code [] [ text textToDisplay ]
        ]


textListToDiv: List String -> List (Html msg)
textListToDiv listToDisplay = 
    map textToDiv listToDisplay