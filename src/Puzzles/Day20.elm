module Day20 exposing (..)

import Data.Day20 exposing (..)


type alias Range =
    ( Float, Float )

maxInt = Result.withDefault 0 (String.toFloat "4294967295")

createRange : List String -> Range
createRange inputList =
    let
        val1 =
            case List.head inputList of
                Just val ->
                    case String.toFloat val of
                        Ok a ->
                            a

                        Err err ->
                            0

                Nothing ->
                    0

        val2 =
            case List.head (List.drop 1 inputList) of
                Just val ->
                    case String.toFloat val of
                        Ok a ->
                            a

                        Err err ->
                            0

                Nothing ->
                    0
    in
        if val1 <= val2 then
            ( val1, val2 )
        else
            ( val2, val1 )


getTestData : List Range
getTestData =
    List.map (\a -> createRange (String.split "-" a)) (String.lines day20TestInput)
        |> List.sort


getPuzzleData : List Range
getPuzzleData =
    List.map (\a -> createRange (String.split "-" a)) (String.lines day20PuzzleInput)
        |> List.sort


mergeSingleRange : Range -> List Range -> List Range
mergeSingleRange ( low, high ) rangeList =
    case List.reverse rangeList of
        [] ->
            [ ( low, high ) ]

        [ ( pLow, pHigh ) ] ->
            if pHigh - low >= -1 then
                [ ( pLow, high ) ]
            else
                [ ( pLow, pHigh ), ( low, high ) ]

        ( pLow, pHigh ) :: rest ->
            if pHigh == maxInt then
                List.append (List.reverse rest) [ ( pLow, pHigh ) ]
            else if pHigh - low >= -1 then
                List.append (List.reverse rest) [ ( pLow, high ) ]
            else
                List.append (List.reverse rest) [ ( pLow, pHigh ), ( low, high ) ]


mergeRanges ranges =
    List.foldl (\a b -> mergeSingleRange a b)
        []
        ranges

countIps: Range -> (Range, Float) -> (Range, Float)
countIps (low, high) ((plow, pHigh), count) =
    let
        _ = Debug.log "" (plow, pHigh, low, high, count)
    in
        case (plow, pHigh) of
            (0,0) ->
                ((low, high),0)
            _ ->
                if high == maxInt then
                    ((low, high), count)
                else
                    ((low, high), count + (low - pHigh))

ipsLeft mergedRangeList =
    let
        ((lastLow, lastHigh), accum) =
            List.foldl (\a b -> countIps a b)
                ( (0,0), 0 )
                mergedRangeList
    in
        accum


-- not 801989041
-- not 797646312