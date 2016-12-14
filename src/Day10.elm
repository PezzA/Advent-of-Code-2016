module Day10 exposing (..)

import Regex exposing (regex, Regex, HowMany, find, Match)
import Array exposing (slice, fromList, Array, get)
import String exposing (toInt)
import Maybe exposing (withDefault)

import Data.Day10 exposing (..)


type Recipient
    = Bot Int
    | Output Int
    | NoRecipient


type Instruction
    = Value Int Recipient
    | WireBot Int Recipient Recipient
    | NoInstruction


commandRegex : Regex
commandRegex =
    Regex.regex "(value|bot|output) ([0-9]+) (goes to|gives low to) (bot|output) ([0-9]+)(( and high to )(bot|output) ([0-9]+))?"


getSubMatch : Array (Maybe String) -> Int -> String
getSubMatch subMatchArray index  =
    case (Array.get index subMatchArray) of
        Just val ->
            Maybe.withDefault "" val

        Nothing ->
            ""


getRecipient : String -> String -> Recipient
getRecipient device index =
    let
        actualIndex =
            Result.withDefault -1 (toInt index)
    in
        case device of
            "bot" ->
                Bot actualIndex

            "output" ->
                Output actualIndex

            _ ->
                NoRecipient


parseMatch : Match -> Instruction
parseMatch match =
    let  
        subMatchPartial = 
            getSubMatch (Array.fromList match.submatches)
    in
        case (subMatchPartial 0) of
            "value" ->
                Value (Result.withDefault -1 (toInt (subMatchPartial 1)))
                    (getRecipient (subMatchPartial 3) (subMatchPartial 4))

            "bot" ->
                WireBot (Result.withDefault -1 (toInt (subMatchPartial 1)))
                    (getRecipient (subMatchPartial 3) (subMatchPartial 4))
                    (getRecipient (subMatchPartial 7) (subMatchPartial 8))

            _ ->
                NoInstruction


doTest =
    List.map parseMatch
        (find (Regex.All) commandRegex day10TestInput)


doPuzzleInput =
    List.map parseMatch
        (find (Regex.All) commandRegex day10PuzzleInput)
