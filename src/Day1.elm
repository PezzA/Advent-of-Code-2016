--Remove the module line if you want to use this in /try


module Day1 exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import String exposing (split)
import Array exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


{-
   Elm architecture section.  Contains all stuff required for elm Html program and any specific UI concerns.
-}


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


init : ( Model, Cmd Msg )
init =
    ( Model (instructions puzzleInput) 0 (Setup 1800 800 3) [([], 0)] ([], 0) False, Cmd.none )


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            updatePaths model 1

        Back ->
            updatePaths model -1

        Forward ->
            updatePaths model 1

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


drawGrid : Int -> Int -> Int -> List (Svg msg)
drawGrid gridX gridY step =
    (rect (rectHelper 0 0 gridX gridY canvasBackGroundStyle) []
        :: (Array.toList
                (Array.map (\a -> line (lineHelper a 0 a gridY roadStyle) [])
                    (initialize (ceiling ((toFloat gridX) / (toFloat step))) (\n -> n * step))
                )
           )
        ++ (Array.toList
                (Array.map (\a -> line (lineHelper 0 a gridX a roadStyle) [])
                    (initialize (ceiling ((toFloat gridY) / (toFloat step))) (\n -> n * step))
                )
           )
    )


canvasBackGroundStyle =
    [ fill "#DDDDDD"
    , stroke "#000000"
    ]


roadStyle =
    [ strokeWidth "1"
    , stroke "#FFFFFF"
    ]

currentPathStyle =
    [ strokeWidth "1"
    , stroke "#0000FF"
    ]

pathStyle =
    [ strokeWidth "1"
    , stroke "#FF0000"
    ]


rectHelper : Int -> Int -> Int -> Int -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
rectHelper iX iY iWidth iHeight styles =
    [ x (toString iX)
    , y (toString iY)
    , width (toString iWidth)
    , height (toString iHeight)
    ]
        ++ styles


lineHelper : Int -> Int -> Int -> Int -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
lineHelper ix1 iy1 ix2 iy2 styles =
    [ x1 (toString ix1)
    , y1 (toString iy1)
    , x2 (toString ix2)
    , y2 (toString iy2)
    ]
        ++ styles


drawPositionPath maxX maxY step position style=
    let
        ( ox, oy ) =
            getPositionOrigin (Tuple.first position)

        ( dx, dy ) =
            getPositionDestination (Tuple.first position)

        ( x1, y1 ) =
            convertFromCartesian ox oy maxX maxY step

        ( x2, y2 ) =
            convertFromCartesian dx dy maxX maxY step
    in
        line (lineHelper x1 y1 x2 y2 style) []


drawPath maxX maxY step positions style=
    List.map (\a -> drawPositionPath maxX maxY step a style)
        positions


view : Model -> Html Msg
view model =
    let
        pathDrawer =
            drawPath model.setup.height model.setup.height model.setup.step

        strX =
            toString model.setup.width

        strY =
            toString model.setup.height
    in
        div []
            [ div []
                [ svg [ width strX, height strY, viewBox ("0 0 " ++ strX ++ " " ++ strY), shapeRendering "optimizeSpeed" ]
                    ((drawGrid model.setup.width model.setup.height model.setup.step)
                        ++ pathDrawer model.previousPositions pathStyle
                        ++ pathDrawer [model.currentPosition] currentPathStyle
                    )
                ]
            , div []
                [ button [ onClick Forward ] [ Html.text "Forward Step" ]
                , button [ onClick Back ] [ Html.text "Back Step" ]
                , div [] [ Html.text ("Frame:" ++ (toString model.frame)) ]
                , div [] [ Html.text ("CrossedPath:" ++ (toString model.crossedPath)) ]
                ]
            ]


convertFromCartesian iX iY iGridX iGridY iStep =
    ( (iGridX // 2) + (iX * iStep), (iGridY // 2) + ((iY * -1) * iStep) )



{-
   Puzzle processing:  All the stuff below here is specific to the puzzle.  Would normally have this in a separate
   module, but it's all in-line so the whole thing can be pasted in /try
-}


type alias Point =
    ( Int, Int )


type alias Position =
    ( List Point, Int )


testOne =
    "R2, L3"


testTwo =
    "R2, R2, R2"


testThree =
    "R5, L5, R5, R3"


puzzleInput =
    "R1, R1, R3, R1, R1, L2, R5, L2, R5, R1, R4, L2, R3, L3, R4, L5, R4, R4, R1, L5, L4, R5, R3, L1, R4, R3, L2, L1, R3, L4, R3, L2, R5, R190, R3, R5, L5, L1, R54, L3, L4, L1, R4, R1, R3, L1, L1, R2, L2, R2, R5, L3, R4, R76, L3, R4, R191, R5, R5, L5, L4, L5, L3, R1, R3, R2, L2, L2, L4, L5, L4, R5, R4, R4, R2, R3, R4, L3, L2, R5, R3, L2, L1, R2, L3, R2, L1, L1, R1, L3, R5, L5, L1, L2, R5, R3, L3, R3, R5, R2, R5, R5, L5, L5, R2, L3, L5, L2, L1, R2, R2, L2, R2, L3, L2, R3, L5, R4, L4, L5, R3, L4, R1, R3, R2, R4, L2, L3, R2, L5, R5, R4, L2, R4, L1, L3, L1, L3, R1, R2, R1, L5, R5, R3, L3, L3, L2, R4, R2, L5, L1, L1, L5, L4, L1, L1, R1"


instructions : String -> List String
instructions input =
    String.split "," input
        |> List.map String.trim


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
