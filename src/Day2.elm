module Day2 exposing (..)

import String
import Array exposing (fromList, length, Array, get, initialize)
import List exposing (foldr)
import Data.Day2 exposing (testData, puzzleInput)


type alias KeyPad =
    Array (Array String)


type alias KeyRow =
    Array String


type alias Key =
    String


type alias Position =
    ( Int, Int )


keyPadSpec : KeyPad
keyPadSpec =
    fromList
        [ fromList [ "0", "0", "0", "0", "0" ]
        , fromList [ "0", "1", "2", "3", "0" ]
        , fromList [ "0", "4", "5", "6", "0" ]
        , fromList [ "0", "7", "8", "9", "0" ]
        , fromList [ "0", "0", "0", "0", "0" ]
        ]


starPadSpec : KeyPad
starPadSpec =
    fromList
        [ fromList [ "0", "0", "0", "0", "0", "0", "0" ]
        , fromList [ "0", "0", "0", "1", "0", "0", "0" ]
        , fromList [ "0", "0", "2", "3", "4", "0", "0" ]
        , fromList [ "0", "5", "6", "7", "8", "9", "0" ]
        , fromList [ "0", "0", "A", "B", "C", "0", "0" ]
        , fromList [ "0", "0", "0", "D", "0", "0", "0" ]
        , fromList [ "0", "0", "0", "0", "0", "0", "0" ]
        ]


keyPos : Position
keyPos =
    ( 2, 2 )


starPos : Position
starPos =
    ( 3, 3 )


getRow : KeyPad -> Int -> KeyRow
getRow pad index =
    case get index pad of
        Just val ->
            val

        Nothing ->
            initialize (length pad) (always "0")


getCol : KeyRow -> Int -> Key
getCol row index =
    case get index row of
        Just val ->
            val

        Nothing ->
            "0"


getKey : KeyPad -> Position -> Key
getKey keyPad ( x, y ) =
    let
        row =
            getRow keyPad y

        key =
            getCol row x
    in
        key


move : KeyPad -> String -> Position -> Position
move keyPad instruction ( x, y ) =
    case instruction of
        "U" ->
            shiftUp keyPad ( x, y )

        "D" ->
            shiftDown keyPad ( x, y )

        "L" ->
            shiftLeft keyPad ( x, y )

        "R" ->
            shiftRight keyPad ( x, y )

        _ ->
            ( 0, 0 )


shiftUp : KeyPad -> Position -> Position
shiftUp keyPad ( x, y ) =
    case getKey keyPad ( x, y - 1 ) of
        "0" ->
            ( x, y )

        _ ->
            ( x, y - 1 )


shiftDown : KeyPad -> Position -> Position
shiftDown keyPad ( x, y ) =
    case getKey keyPad ( x, y + 1 ) of
        "0" ->
            ( x, y )

        _ ->
            ( x, y + 1 )


shiftLeft : KeyPad -> Position -> Position
shiftLeft keyPad ( x, y ) =
    case getKey keyPad ( x - 1, y ) of
        "0" ->
            ( x, y )

        _ ->
            ( x - 1, y )


shiftRight : KeyPad -> Position -> Position
shiftRight keyPad ( x, y ) =
    case getKey keyPad ( x + 1, y ) of
        "0" ->
            ( x, y )

        _ ->
            ( x + 1, y )


processLine : KeyPad -> String -> Position -> Position
processLine pad inputData ( x, y ) =
    let
        commands =
            String.toList inputData
                |> List.map String.fromChar
    in
        List.foldl (move pad) ( x, y ) commands


processSequence : KeyPad -> String -> List Position
processSequence keyPad inputData =
    List.scanl (processLine keyPad) keyPos (String.lines inputData)
        |> List.drop 1


displaySequence : KeyPad -> String -> List String
displaySequence pad instructions =
    List.map (getKey pad) (processSequence pad instructions)


doTest =
    displaySequence keyPadSpec testData


doPartOne =
    displaySequence keyPadSpec puzzleInput


doPartTwo =
    displaySequence starPadSpec puzzleInput
