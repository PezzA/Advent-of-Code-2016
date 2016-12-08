module Day7 exposing (..)

import Html exposing (text, div)
import String exposing (lines, slice, length, indices)
import List exposing (map, any, filter, map2, foldl)
import Data.Day7 exposing (data)


splitHypernetSequence ( start, end ) ( partString, hypernetSequences, offset ) =
    let
        stringToChop =
            slice (start - offset) ((end - offset) + 1) partString

        newString =
            slice 0 (start - offset) partString ++ "|" ++ slice ((end - offset) + 1) (length partString) partString

        newOffset =
            offset + (end - start)
    in
        ( newString, stringToChop :: hypernetSequences, newOffset )


getHypernetSequences : String -> ( String, List String, Int )
getHypernetSequences input =
    let
        markers =
            map2 (,) (indices "[" input) (indices "]" input)
    in
        foldl splitHypernetSequence ( input, [], 0 ) markers


isAbba : String -> Bool
isAbba token =
    (slice 0 1 token) == (slice 3 4 token)
        && (slice 1 2 token) == (slice 2 3 token)
        && (slice 0 1 token) /= (slice 1 2 token)


hasAbba : String -> Bool
hasAbba input =
    let
        maxLen =
            (length input) - 4
    in
        map (\a -> isAbba (slice a (a + 4) input)) [0..maxLen]
            |> any (\a -> a)


tlsList : List String -> List ( String, List String, Int )
tlsList input =
    map (\a -> (getHypernetSequences a)) input
        |> filter (\ ( ip, hypernetSequences, c) -> hasAbba ip && not (List.any hasAbba hypernetSequences))

main =
    div []
    [ div [] [ text (toString (List.length (tlsList (lines data)))) ]
    , div [] (map (\ a -> div [] [ text (toString a) ] ) (tlsList (lines data)))
    ]
