module Day1 exposing (..)

import String
import Array exposing (..)

instructions : String -> Array String
instructions input =
    String.split "," input
        |> List.map String.trim
        |> Array.fromList


turnLeft : Int -> Int
turnLeft orientation =
    case orientation of
        0 ->
            270

        90 ->
            0

        180 ->
            90

        270 ->
            180

        _ ->
            0


turnRight : Int -> Int
turnRight orientation =
    case orientation of
        0 ->
            90

        90 ->
            180

        180 ->
            270

        270 ->
            0

        _ ->
            0


orientate : String -> Int -> Int
orientate direction orientation =
    case direction of
        "L" ->
            turnLeft orientation

        "R" ->
            turnRight orientation

        _ ->
            0


translate : String -> Int -> Int -> ( Int, Int ) -> ( Int, Int )
translate direction orientation shift ( x, y ) =
    case direction of
        "L" ->
            translateLeft orientation shift ( x, y )

        "R" ->
            translateRight orientation shift ( x, y )

        _ ->
            ( 0, 0 )


translateLeft : Int -> Int -> ( Int, Int ) -> ( Int, Int )
translateLeft orientation shift ( x, y ) =
    case orientation of
        0 ->
            ( x , y - shift )

        90 ->
            ( x - 1, y)

        180 ->
            ( x , y + shift)

        270 ->
            ( x + shift, y )

        _ ->
            ( 0, 0 )


translateRight : Int -> Int -> ( Int, Int ) -> ( Int, Int )
translateRight orientation shift ( x, y ) =
    case orientation of
        0 ->
            ( x , y + shift )

        90 ->
            ( x + shift, y)

        180 ->
            ( x, y - shift )

        270 ->
            ( x - shift, y )

        _ ->
            ( 0, 0 )


parseInstruction : String -> ( String, Int )
parseInstruction input =
    let
        direction =
            String.left 1 input

        shift =
            case String.toInt (String.dropLeft 1 input) of
                Ok val ->
                    val

                Err msg ->
                    0
    in
        ( direction, shift )


move : String -> ( Int, Int, Int ) -> ( Int, Int, Int )
move instruction ( x, y, orientation ) =
    let
        ( direction, shift ) =
            parseInstruction instruction

        newOrientation =
            orientate direction orientation

        ( newX, newY ) =
            Array.foldr (\a (b, c) -> translate direction newOrientation 1 ( b, c )) (x,y) (fromList [1..shift])
        _ = Debug.log ":" instruction
        _ = Debug.log ":" (List.drop 1 (List.scanl (\a (b, c) -> translate direction newOrientation 1 ( b, c )) (x,y) [1..shift]))
    in
        ( newX, newY, newOrientation )


getBlocks : ( Int, Int, Int ) -> Float
getBlocks ( x, y, orientation ) =
    (abs (toFloat x)) + (abs (toFloat y))


runCommands : String -> Float
runCommands input =
    getBlocks (Array.foldr move ( 0, 0, 0 ) (instructions input))
