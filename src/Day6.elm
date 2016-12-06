module Day6 exposing (..)

import Html exposing (text, div, h1, Html)
import String exposing (lines, slice)

import Data.Day6 exposing (day6Data)

alphabet = 
    "abcdefghijklmnopquvrstwxyz"
        |> String.toList
        |> List.map String.fromChar
        

getCol: Int -> Int -> List String -> List String
getCol start end lines = 
    List.map (\a -> String.slice start end a ) lines
    
    
getSortedCol: Int -> Int -> List String ->  List String -> List ( Int, String )
getSortedCol start end lines alphaList = 
    List.map (getCount (getCol start end lines)) alphaList
        |> List.sort

-- Took out the reverse to get the part 2 bit.  would be nice to specify each way


getCount: List String -> String -> ( Int, String )
getCount colData compare = 
    let
        count = 
            List.filter (\ a -> a == compare ) colData  
                |> List.length
    in
        ( count, compare )

getColChar: Int -> Int -> List String ->  List String -> String
getColChar start end lines alphaList = 
    let
        sortedArray = getSortedCol start end lines alphaList 
       
        (x, y ) = 
            case List.head sortedArray of
                Just val -> val
                Nothing -> ( 0 , "" )
    in 
        y
        
main = 
    div [] 
        [ h1 [] [ text "Day 6"]
        , div [] [ text ( getColChar 0 1 (String.lines day6Data) alphabet ) ]
        , div [] [ text ( getColChar 1 2 (String.lines day6Data) alphabet ) ]
        , div [] [ text ( getColChar 2 3 (String.lines day6Data) alphabet ) ]
        , div [] [ text ( getColChar 3 4 (String.lines day6Data) alphabet ) ]
        , div [] [ text ( getColChar 4 5 (String.lines day6Data) alphabet ) ]
        , div [] [ text ( getColChar 5 6 (String.lines day6Data) alphabet ) ]
        , div [] [ text ( getColChar 6 7 (String.lines day6Data) alphabet ) ]        
        , div [] [ text ( getColChar 7 8 (String.lines day6Data) alphabet ) ]        
        ]
