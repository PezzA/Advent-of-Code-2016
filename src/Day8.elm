module Day8 exposing (..)

import String exposing (lines, append)
import Array exposing (repeat, Array, map, foldr, slice, indexedMap, length, fromList, get)
import Data.Day8 exposing (day8Data)
import Regex exposing (..)
import Common exposing (renderPixel)


type Instruction
    = Rect Int Int
    | RotateCol Int Int
    | RotateRow Int Int
    | NoOp


commandRegex : Regex
commandRegex =
    regex "(rotate row) y=(\\d.*) by (\\d.*)|(rotate column) x=(\\d.*) by (\\d.*)|(rect) (\\d.*)x(\\d.*)"


arrayBitToInt index array =
    case get index array of
        Just arraybit ->
            case arraybit of
                Just stringval ->
                    case String.toInt stringval of
                        Ok parsedVal ->
                            parsedVal

                        Err err ->
                            -1

                Nothing ->
                    -1

        Nothing ->
            -1


extractInstruction match =
    let
        bits =
            fromList (List.filter (\a -> a /= Nothing) match.submatches)

        ins =
            case get 0 bits of
                Just val ->
                    case val of
                        Just val ->
                            val

                        Nothing ->
                            ""

                Nothing ->
                    ""

        val1 =
            arrayBitToInt 1 bits

        val2 =
            arrayBitToInt 2 bits
    in
        case ins of
            "rect" ->
                Rect val1 val2

            "rotate row" ->
                RotateRow val1 val2

            "rotate column" ->
                RotateCol val1 val2

            _ ->
                NoOp


getInstructions data =
    let
        matches =
            Regex.find (All) commandRegex data

        ins =
            List.map (\a -> extractInstruction a) matches
    in
        fromList ins


type alias Display =
    Array (Array Bool)


blankDisplay : Display
blankDisplay =
    repeat 6 (repeat 50 False)


renderLine : Array Bool -> String
renderLine displayData =
    foldr (\a -> append (renderPixel a)) "" displayData


renderDisplay : Display -> List String
renderDisplay display =
    map renderLine display
        |> Array.toList


consoleDisplay : Display -> Array String
consoleDisplay display =
    Array.map (\a -> Debug.log ":" (renderLine a)) display


rect : Int -> Int -> Display -> Display
rect x y displayData =
    indexedMap
        (\a b ->
            if a < y then
                indexedMap
                    (\c d ->
                        if c < x then
                            True
                        else
                            d
                    )
                    b
            else
                b
        )
        displayData


shiftedPos : Int -> Int -> Int -> Int
shiftedPos val shift length =
    (val - (shift - length)) % length


getPixel : Display -> Int -> Int -> Bool
getPixel display x y =
    let
        col =
            case get y display of
                Just val ->
                    val

                Nothing ->
                    repeat 50 False
    in
        case get x col of
            Just val ->
                val

            Nothing ->
                False


setPixel : Display -> Int -> Int -> Bool -> Display
setPixel displayData x y val =
    Array.indexedMap
        (\i a ->
            if i == x then
                Array.set y val a
            else
                a
        )
        displayData


rotateCol col shift displayData =
    let
        actualShift =
            shift % (Array.length displayData)

        len =
            Array.length displayData
    in
        Array.foldr (\row disp -> setPixel disp row col (getPixel displayData col (shiftedPos row actualShift len)))
            displayData
            (fromList (List.range 0 (len - 1)))


rotateRow row shift displayData =
    let
        targetRow =
            case Array.get row displayData of
                Just val ->
                    val

                Nothing ->
                    repeat 50 False

        len =
            Array.length targetRow

        actualShift =
            shift % len

        updatedRow =
            Array.indexedMap
                (\index val ->
                    case Array.get (shiftedPos index actualShift len) targetRow of
                        Just val ->
                            val

                        Nothing ->
                            False
                )
                targetRow
    in
        Array.set row updatedRow displayData


drawPart instruction display =
    case instruction of
        Rect x y ->
            rect x y display

        RotateRow row shift ->
            rotateRow row shift display

        RotateCol col shift ->
            rotateCol col shift display

        NoOp ->
            display


doPartOne data =
    Array.foldl (\ins display -> drawPart ins display) blankDisplay (getInstructions data)


pixelCountDisplay display =
    foldr (\row count -> count + length ((Array.filter (\b -> b) row))) 0 display



-- pixelCountDisplay (doPartOne day8Data
-- doPartOne day8Data
