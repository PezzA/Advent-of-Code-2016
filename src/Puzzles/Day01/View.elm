module Puzzles.Day01.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (..)
import Puzzles.Day01.Model exposing (..)
import Puzzles.Day01.Styles exposing (..)
import Shared.Charting exposing (..)


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


drawPositionPath maxX maxY step position style =
    let
        ( ox, oy ) =
            getJumpOrigin (Tuple.first position)

        ( dx, dy ) =
            getJumpDestination (Tuple.first position)

        ( x1, y1 ) =
            convertFromCartesian ox oy maxX maxY step

        ( x2, y2 ) =
            convertFromCartesian dx dy maxX maxY step
    in
        line (lineHelper x1 y1 x2 y2 style) []


drawPath maxX maxY step positions style =
    List.map (\a -> drawPositionPath maxX maxY step a style)
        positions


drawBoundingBox : Model -> Svg Msg
drawBoundingBox model =
    case model.boundingBox of
        ( Nothing, Nothing, Nothing, Nothing ) ->
            Svg.text ""

        ( a, b, c, d ) ->
            let
                tx =
                    (Maybe.withDefault 0 a) + 1

                bx =
                    (Maybe.withDefault 0 b) - 1

                by =
                    (Maybe.withDefault 0 c) + 1

                ty =
                    (Maybe.withDefault 0 d) - 1
            in
                rect
                    (rectHelper (convertXAxisFromCartesian bx model.setup.width model.setup.step)
                        (convertYAxisFromCartesian by model.setup.height model.setup.step)
                        (abs ((tx - bx) * model.setup.step))
                        (abs ((ty - by) * model.setup.step))
                        boundingStyle
                    )
                    []


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
                        ++ [ drawBoundingBox model ]
                        ++ pathDrawer model.previousJumps pathStyle
                        ++ pathDrawer [ model.currentJump ] currentPathStyle
                    )
                ]
            , div []
                [ button [ onClick Forward ] [ Html.text "Forward Step" ]
                , button [ onClick Back ] [ Html.text "Back Step" ]
                , div [] [ Html.text ("Frame:" ++ (toString model.frame)) ]
                , div [] [ Html.text ("CrossedPath:" ++ (toString model.crossedPath)) ]
                ]
            ]
