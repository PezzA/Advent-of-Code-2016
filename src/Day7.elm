module Day7 exposing (..)

import String exposing (lines, slice, length, indices)
import List exposing (map, any, filter, map2, foldl, append)
import Data.Day7 exposing (data)


type alias ParsedIpAddress =
    { ipSequences : List String
    , hypernetSequences : List String
    }


parseIpAddress : String -> ParsedIpAddress
parseIpAddress input =
    let
        markers =
            map2 (,)
                (indices "[" input)
                (indices "]" input)

        outMarkers =
            map2 (,)
                (append (indices "[" input) [ (length input) ])
                (0 :: (map (\a -> a + 1) (indices "]" input)))
    in
        ParsedIpAddress (map (\( a, b ) -> slice (b) a input) outMarkers)
            (map (\( a, b ) -> slice (a + 1) b input) markers)


isAbba : String -> Bool
isAbba token =
    ((slice 0 1 token) == (slice 3 4 token))
        && ((slice 1 2 token) == (slice 2 3 token))
        && ((slice 0 1 token) /= (slice 1 2 token))


isAba : String -> Bool
isAba token =
    ((slice 0 1 token) == (slice 2 3 token))
        && ((slice 0 1 token) /= (slice 1 2 token))


hasAbba : String -> Bool
hasAbba input =
    let
        maxLen =
            (length input) - 4
    in
        map (\a -> isAbba (slice a (a + 4) input)) [0..maxLen]
            |> any (\a -> a)


hasAba : String -> Bool
hasAba input =
    let
        maxLen =
            (length input) - 3
    in
        map (\a -> isAba (slice a (a + 3) input)) [0..maxLen]
            |> any (\a -> a)


getAbas : String -> List String
getAbas input =
    let
        maxLen =
            (length input) - 3
    in
        map (\a -> ( (slice a (a + 3) input), isAba (slice a (a + 3) input) )) [0..maxLen]
            |> filter (\( a, b ) -> b)
            |> map (\( a, b ) -> a)


reciprocalAba : String -> String
reciprocalAba input =
    slice 1 2 input ++ slice 0 1 input ++ slice 1 2 input


supportsTls : ParsedIpAddress -> Bool
supportsTls parsedIp =
    (List.any hasAbba parsedIp.ipSequences)
        && not (List.any hasAbba parsedIp.hypernetSequences)


supportsSsl : ParsedIpAddress -> Bool
supportsSsl parsedIp =
    let
        ipAbas =
            List.concat (List.map (\a -> getAbas a) parsedIp.ipSequences)

        hnAbas =
            List.concat (List.map (\a -> getAbas a) parsedIp.hypernetSequences)
    in
        any (\a -> a) (map (\a -> any (\b -> (reciprocalAba a) == b) hnAbas) ipAbas)


doPartOne =
    map (\a -> supportsTls (parseIpAddress a)) (lines data)
        |> filter (\a -> a)
        |> List.length


doPartTwo =
    map (\a -> supportsSsl (parseIpAddress a)) (lines data)
        |> filter (\a -> a)
        |> List.length
