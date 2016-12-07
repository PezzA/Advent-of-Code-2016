module Day7 exposing (..)
import Html exposing (text, div)
import String exposing (lines, slice, length, indices)
import List exposing (map, any, filter, map2, foldl )

import Data.Day7 exposing (data)

removeRelativeString (start, end) (partString, offset) = 
    let 
        newString = 
            slice 0 (start - offset) partString ++ slice ((end - offset) +1) (length partString) partString
    in
        (newString, offset + (end - start) + 1 )

removeHypernetSequences: String -> (String , Int)
removeHypernetSequences input = 
    let
        markers = map2 (,) (indices "[" input) (indices "]" input)
    in
        foldl removeRelativeString (input, 0) markers
       
       
isAbba: String -> Bool
isAbba token =
    (slice 0 1 token) ==  (slice 3 4 token) && (slice 1 2 token) ==  (slice 2 3 token)


supportsTls: String -> Bool       
supportsTls input = 
    let
        maxLen = (length input) - 4        
    in 
        map (\ a -> isAbba(slice a (a + 4) input)) [0..maxLen]
            |> any (\a -> a)


tlsList: List String -> List Bool
tlsList input = 
    map (\a -> supportsTls (fst(removeHypernetSequences a)) ) input
        |> filter (\a -> a)
    

main = 
    div [] [ text (toString (List.length (tlsList (lines data))))]