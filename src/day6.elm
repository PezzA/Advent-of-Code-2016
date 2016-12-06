module Day6 exposing (..)

import Html exposing (text, div, h1, Html)
import String exposing (lines)

import Data.Day6 exposing (day6Data)


testRecord: String -> Html msg
testRecord input = 
    div [] [ text input ]


testDataRows: List String -> List (Html msg)
testDataRows inputData = 
    List.map testRecord inputData
    
    
processInputData: String -> List String
processInputData input = 
    String.lines input
    
    
main = 
    div [] 
        [ h1 [] [ text "Day 6"]
        , div [] (testDataRows (processInputData day6Data))
        ]