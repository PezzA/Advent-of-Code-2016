module Day01.View exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Array exposing (..)

import Day01.Model exposing (..)

convertFromCartesian iX iY iGridX iGridY iStep =
    ( (iGridX // 2) + (iX * iStep), (iGridY // 2) + ((iY * -1) * iStep) )

    
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
