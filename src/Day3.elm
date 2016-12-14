module Main exposing (..)

import Html exposing (..)
import String
import Common exposing (TestRecord, stringToInt)
import Data.Day3 exposing (day3Data)


type alias Triangle =
    List Int


getHeadTriangle : Triangle -> Int
getHeadTriangle list =
    case List.head list of
        Just val ->
            val

        Nothing ->
            0


stringToTriangle : String -> Triangle
stringToTriangle input =
    String.trim input
        |> String.words
        |> List.map stringToInt


validTriangle : Triangle -> Bool
validTriangle sides =
    let
        sortedSides =
            List.sort sides
    in
        (List.take 2 sortedSides |> List.sum) > getHeadTriangle (List.drop 2 sortedSides)


testResults : String -> List Bool
testResults test =
    List.map validTriangle (List.map stringToTriangle (String.lines test))


successCount : List Bool -> Int
successCount results =
    List.length (List.filter (\a -> a) results)


testTableRow : TestRecord -> Html msg
testTableRow ( name, test ) =
    tr []
        [ td [] [ text name ]
        , td [] [ text (toString (successCount (testResults test))) ]
        ]


getCol : Int -> Triangle -> Int
getCol index tri =
    case List.head (List.drop index tri) of
        Just val ->
            val

        Nothing ->
            0


rowsToCols : List Triangle -> List Triangle
rowsToCols triangles =
    [ List.map (getCol 0) triangles
    , List.map (getCol 1) triangles
    , List.map (getCol 2) triangles
    ]


loadColumnData : List String -> List Triangle
loadColumnData data =
    let
        triangles =
            List.map stringToTriangle (List.take 3 data)
    in
        if List.length data > 0 then
            List.append  (rowsToCols triangles)  (loadColumnData (List.drop 3 data))
        else
            (rowsToCols triangles)

testColResults : String -> List Bool
testColResults test =
    List.map validTriangle (loadColumnData (String.lines test))



testTableRow2 : TestRecord -> Html msg
testTableRow2 ( name, test ) =
    tr []
        [ td [] [ text name ]
         , td [] [ text (toString (successCount (testColResults test))) ]
        ]


main =
    div []
        [ h1 [] [ text "Day Three" ]
        , h2 [] [ text "First Star" ]
        , table [] (List.map testTableRow day3Data)
        , h2 [] [ text "Second Star" ]
        , table [] (List.map testTableRow2 day3Data)
        ]
