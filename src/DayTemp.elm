module DayTemp exposing (..)

import Regex exposing (regex, Regex, HowMany, find, Match)
import Array exposing (slice, fromList, Array)
import String exposing (toInt)
import Maybe exposing (withDefault)
import Data.DayTemp exposing (..)


type Recipient
    = Bot Int
    | Output Int
    | Unknown


type Instruction
    = Value Int Recipient
    | WireBot Int Recipient Recipient
    | NoOp


commandRegex : Regex
commandRegex =
    regex "(value|bot|output) ([0-9]+) (goes to|gives low to) (bot|output) ([0-9]+)(( and high to )(bot|output) ([0-9]+))?"


getSubMatch : Int -> Array (Maybe String) -> String
getSubMatch index subMatchArray =
    case (Array.get index subMatchArray) of
        Just val ->
            withDefault "" val

        Nothing ->
            ""


getRecipient : String -> String -> Recipient
getRecipient device index =
    let
        actualIndex =
            withDefault -1 (toInt index)
    in
        case device of
            "bot" ->
                Bot actualIndex

            "output" ->
                Output actualIndex

            _ ->
                Unknown


parseMatch : Match -> Instruction
parseMatch match =
    let
        matchesArray =
            Array.fromList match.submatches
    in
        case (getSubMatch 0 matchesArray) of
            "value" ->
                Value (withDefault -1 (toInt (getSubMatch 1 matchesArray)))
                    (getRecipient (getSubMatch 3 matchesArray) (getSubMatch 4 matchesArray))

            "bot" ->
                WireBot (withDefault -1 (toInt (getSubMatch 1 matchesArray)))
                    (getRecipient (getSubMatch 3 matchesArray) (getSubMatch 4 matchesArray))
                    (getRecipient (getSubMatch 7 matchesArray) (getSubMatch 8 matchesArray))

            _ ->
                NoOp


doTest =
    List.map parseMatch
        (find (Regex.All) commandRegex day10TestInput)


doPuzzleInput =
    List.map parseMatch
        (find (Regex.All) commandRegex day10PuzzleInput)
