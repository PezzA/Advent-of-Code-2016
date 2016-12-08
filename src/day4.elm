module Main exposing (..)

import Html exposing (..)
import String
import Common exposing (TestRecord, stringToInt)
import Data.Day4 exposing (day4Data)

alphabet : List String
alphabet =
    "abcdefghijklmnopquvrstwxyz"
        |> String.toList
        |> List.map String.fromChar

splitRecord : String -> ( Int, Bool )
splitRecord input =
    let
        array =
            String.split "-" input
                |> List.reverse

        nameParts =
            String.concat (List.drop 1 array)
                |> String.toList
                |> List.sort
                |> String.fromList

        checkSector =
            case List.head array of
                Just val ->
                    val

                Nothing ->
                    ""

        sectorId =
            String.dropRight 7 checkSector
                |> stringToInt

        checkSum =
            String.right 7 checkSector
                |> String.slice 1 6

        occurrences = List.map (checkString nameParts) alphabet
            |> List.sortWith occurenceCompare
            |> List.reverse
            |> List.take 5
            |> List.map snd
            |> String.concat
    in
        ( sectorId,  checkSum == occurrences )

occurenceCompare a b =
    case compare (fst a) (fst b) of
      EQ -> compare (snd b) (snd a)
      order -> order

checkString: String -> String -> ( Int, String )
checkString test compare =
    let
        count = String.toList test
            |> List.map String.fromChar
            |> List.filter (\ a -> a == compare )
            |> List.length
    in
        ( count, compare )


roomChecker : List String -> Int
roomChecker lines =
    List.map splitRecord lines
        |> List.filter (\ a -> snd a)
        |> List.foldr (\ ( a, b) c -> a + c) 0



testResultRow : TestRecord -> Html msg
testResultRow ( name, test ) =
    tr []
        [ td [] [ text name ]
        , td [] [ text (toString (roomChecker (String.lines test))) ]
        ]


main =
    div []
        [ h1 [] [ text "Day Three" ]
        , h2 [] [ text (toString alphabet) ]
        , table [] (List.map testResultRow day4Data)
        , h2 [] [ text "Second Star" ]
        ]
