module Shared.Charting exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

convertFromCartesian iX iY iGridX iGridY iStep =
    ( (iGridX // 2) + (iX * iStep), (iGridY // 2) + ((iY * -1) * iStep) )


convertXAxisFromCartesian iX iGridX iStep =
    (iGridX // 2) + (iX * iStep)


convertYAxisFromCartesian iY iGridY iStep =
    (iGridY // 2) + ((iY * -1) * iStep)

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
