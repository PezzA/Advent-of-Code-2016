module Day01.Model exposing (..)

import Time exposing (Time)
import Array exposing (..)



type alias Point =
    ( Int, Int )


type alias Position =
    ( List Point, Int )

    
type Msg
    = Tick Time
    | Forward
    | Back
    | NoOp

type alias Setup =
    { width :Int
    , height : Int
    , step :Int
    }

type alias Model =
    { instructions : List String
    , frame : Int
    , setup : Setup
    , previousPositions : List Position
    , currentPosition : Position
    , crossedPath : Bool
    }
    
getPositionOrigin : List Point -> Point
getPositionOrigin moves =
    let
        item =
            List.head moves
    in
        case item of
            Just val ->
                val

            Nothing ->
                ( 0, 0 )


getPositionDestination : List Point -> Point
getPositionDestination moves =
    let
        item =
            List.reverse moves
                |> List.head
    in
        case item of
            Just val ->
                val

            Nothing ->
                ( 0, 0 )
                

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


translate : Int -> Int -> ( Int, Int ) -> Point
translate orientation shift ( x, y ) =
    case orientation of
        0 ->
            ( x, y + shift )

        90 ->
            ( x + shift, y )

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


updatePaths model modifier =
    let
        -- get new frame value
        newFrame =
            (clamp 0 (List.length model.instructions) (model.frame + modifier))

        -- get all previous paths (this is to compare new positions with)
        previousPaths =
            List.scanl move ( [], 0 ) (List.take model.frame model.instructions)

        -- put ins list into an array for easier slicing.
        insArray =
            Array.fromList model.instructions

        -- get the curent instruction
        currentIns =
            Array.slice model.frame (model.frame + 1) insArray
                |> Array.get 0
                |> (\a ->
                        case a of
                            Just val ->
                                val

                            Nothing ->
                                ""
                   )

        lastPosition =
            List.reverse previousPaths
                |> List.head
                |> (\a ->
                        case a of
                            Just val ->
                                val

                            Nothing ->
                                ( [], 0 )
                   )

        newPosition =
            move currentIns lastPosition

        allPaths =
            List.foldr (\a b -> (List.drop 1 (Tuple.first a)) ++ b)
                []
                previousPaths


        crossedPathDetail =
            List.map
                (\a -> (a, (List.member a allPaths)) )
                ((List.drop 1 ((Tuple.first) newPosition)))
                |> List.filter (\a -> Tuple.second a)

        _ = Debug.log ":" crossedPathDetail
        crossedPath =
            if List.length crossedPathDetail > 0 then
                True
            else
                False

        allPositions =
            List.append previousPaths [ newPosition ]
    in
        ( { model | frame = newFrame, previousPositions = allPositions, currentPosition = newPosition, crossedPath = crossedPath }, Cmd.none )



move : String -> Position -> Position
move instruction ( moves, orientation ) =
    let
        ( direction, shift ) =
            parseInstruction instruction

        newOrientation =
            orientate direction orientation

        moveList =
            List.scanl (\a ( b, c ) -> translate newOrientation 1 ( b, c ))
                (getPositionDestination moves)
                (List.range 1 shift)
    in
        ( moveList, newOrientation )


getBlocks : ( Int, Int, Int ) -> Float
getBlocks ( x, y, orientation ) =
    (abs (toFloat x)) + (abs (toFloat y))
