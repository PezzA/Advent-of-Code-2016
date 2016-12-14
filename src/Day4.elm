module Day4 exposing (..)

import String exposing (toList, fromList, fromChar, split, concat, dropRight, right, slice, contains, lines, join)
import Char exposing (fromCode, toCode)
import List exposing (map, reverse, drop, sort, head, filter, foldr, length, take, sortWith)
import Common exposing (stringToInt)
import Data.Day4 exposing (puzzleInput, partOneTest)


type alias ParsedName =
    { sectorId : Int
    , decryptedName : String
    , isRealRoom : Bool
    }


alphabet : List String
alphabet =
    "abcdefghijklmnopquvrstwxyz"
        |> toList
        |> map fromChar


splitRecord : String -> ParsedName
splitRecord input =
    let
        array =
            split "-" input
                |> reverse

        nameParts =
            concat (drop 1 array)
                |> toList
                |> sort
                |> fromList

        checkSector =
            case head array of
                Just val ->
                    val

                Nothing ->
                    ""

        sectorId =
            dropRight 7 checkSector
                |> stringToInt

        checkSum =
            right 7 checkSector
                |> slice 1 6

        occurrences =
            map (checkString nameParts) alphabet
                |> sortWith occurenceCompare
                |> reverse
                |> take 5
                |> map snd
                |> concat
    in
        ParsedName sectorId (rotateString sectorId (join "-" (reverse (drop 1 array)))) (checkSum == occurrences)


occurenceCompare a b =
    case compare (fst a) (fst b) of
        EQ ->
            compare (snd b) (snd a)

        order ->
            order


checkString : String -> String -> ( Int, String )
checkString test compare =
    let
        count =
            toList test
                |> map fromChar
                |> filter (\a -> a == compare)
                |> length
    in
        ( count, compare )


roomChecker : List String -> Int
roomChecker lines =
    map splitRecord lines
        |> filter (\a -> a.isRealRoom)
        |> foldr (\a b -> a.sectorId + b) 0


roomNameChecker : List String -> List ParsedName
roomNameChecker lines =
    map splitRecord lines
        |> filter (\a -> contains "northpole" a.decryptedName)


rotateLetter : Int -> Char -> Char
rotateLetter shift input =
    let
        newCode =
            (toCode input) + shift
    in
        if newCode < 123 then
            fromCode newCode
        else
            fromCode (newCode - 26)


decodeLetter : Int -> Char -> Char
decodeLetter shift input =
    case input of
        '-' ->
            ' '

        _ ->
            rotateLetter (shift % 26) input


rotateString : Int -> String -> String
rotateString shift input =
    map (decodeLetter shift) (toList input)
        |> fromList


doTestOne =
    roomChecker (lines partOneTest)


doPartOne =
    roomChecker (lines puzzleInput)


doPartTwo =
    roomNameChecker (lines puzzleInput)
