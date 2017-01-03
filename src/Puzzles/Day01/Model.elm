module Puzzles.Day01.Model exposing (..)

import Array exposing (..)


type alias Point =
    ( Int, Int )


type alias Jump =
    ( List Point, Int )


emptyJump : Jump
emptyJump =
    ( [], 0 )


type Msg
    = Forward
    | Back
    | NoOp


type alias Setup =
    { width : Int
    , height : Int
    , step : Int
    }


type alias Box =
    ( Maybe Int, Maybe Int, Maybe Int, Maybe Int )


newBox : Box
newBox =
    ( Nothing, Nothing, Nothing, Nothing )


type alias Model =
    { instructions : List ( Int, String )
    , frame : Int
    , setup : Setup
    , previousJumps : List Jump
    , currentJump : Jump
    , crossedPath : Bool
    , boundingBox : Box
    , currentInstruction : String
    }


newModel : List ( Int, String ) -> Setup -> Model
newModel instructions setup =
    Model instructions 0 setup [] emptyJump False newBox ""


getJumpOrigin : List Point -> Point
getJumpOrigin moves =
    let
        item =
            List.head moves
    in
        case item of
            Just val ->
                val

            Nothing ->
                ( 0, 0 )


getJumpDestination : List Point -> Point
getJumpDestination moves =
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


orientate : String -> Int -> Int
orientate direction orientation =
    case direction of
        "L" ->
            case orientation of
                0 ->
                    270

                _ ->
                    orientation - 90

        "R" ->
            case orientation of
                270 ->
                    0

                _ ->
                    orientation + 90

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


updatePaths : Model -> Int -> Model
updatePaths model modifier =
    let
        -- get new frame value
        newFrame =
            clamp 0 (List.length model.instructions) (model.frame + modifier)

        currentInstruction =
            case List.filter (\( a, b ) -> a == (newFrame - 1)) model.instructions of
                [ ( a, b ) ] ->
                    b

                _ ->
                    ""

        -- get all previous paths (this is to compare new Jumps with)
        allJumps =
            List.scanl (\ins ( a, b ) -> move ins (getJumpDestination a) b)
                emptyJump
                (List.take model.frame (List.map (\( a, b ) -> b) model.instructions))

        ( lastSegments, lastOrientation ) =
            case List.reverse allJumps of
                [] ->
                    emptyJump

                [ x ] ->
                    x

                x :: _ ->
                    x

        newJump =
            move currentInstruction (getJumpDestination lastSegments) lastOrientation

        allJumpSegments =
            List.foldr (\a b -> (List.drop 1 (Tuple.first a)) ++ b)
                []
                allJumps

        crossedPathDetail =
            List.map (\a -> ( a, (List.member a allJumpSegments) ))
                ((List.drop 1 ((Tuple.first) newJump)))
                |> List.filter (\a -> Tuple.second a)

        crossedPath =
            if List.length crossedPathDetail > 0 then
                True
            else
                False

        finalJumps =
            List.append allJumps [ newJump ]

        finalJumpSegments =
            List.foldr (\a b -> (Tuple.first a ++ b))
                []
                finalJumps


        ( bx, tx, by, ty ) =
            ( List.maximum (List.map (\( a, b ) -> a) finalJumpSegments)
            , List.minimum (List.map (\( a, b ) -> a) finalJumpSegments)
            , List.maximum (List.map (\( a, b ) -> b) finalJumpSegments)
            , List.minimum (List.map (\( a, b ) -> b) finalJumpSegments)
            )

        -- need to set the setp to draw a grid that will enclose just current path, setp sixed of biggest side.  Pan?
        xLen =
            abs ((Maybe.withDefault 0 tx) - bx)

        yLen =
            abs (ty - by)

        newStep =
            if (xLen <= yLen) then
                xLen / (toFloat model.setup.width)
            else
                xLen / (toFloat model.setup.height)
    in
        if newFrame == 0 then
            { model | frame = newFrame, previousJumps = [], currentJump = emptyJump, crossedPath = False, currentInstruction = "", boundingBox = ( Nothing, Nothing, Nothing, Nothing ) }
        else
            { model | frame = newFrame, previousJumps = finalJumps, currentJump = newJump, crossedPath = crossedPath, currentInstruction = currentInstruction, boundingBox = ( bx, tx, by, ty ) }


move : String -> Point -> Int -> Jump
move instruction point orientation =
    let
        ( direction, shift ) =
            parseInstruction instruction

        newOrientation =
            orientate direction orientation

        moveList =
            List.scanl (\a ( b, c ) -> translate newOrientation 1 ( b, c ))
                point
                (List.range 1 shift)
    in
        ( moveList, newOrientation )


getBlocks : ( Int, Int, Int ) -> Float
getBlocks ( x, y, orientation ) =
    (abs (toFloat x)) + (abs (toFloat y))
