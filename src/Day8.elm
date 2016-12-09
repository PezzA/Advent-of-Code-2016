module Day8 exposing (..)

import Html exposing (div, code)
import String exposing (lines, append)
import Array exposing (repeat, Array, map, foldr, slice, indexedMap)

import Data.Day8 exposing (day8Data)
import Common exposing (textListToDiv)

type alias Display = Array (Array Bool)

  
display : Display
display = 
    repeat 6 ( repeat 50 False)

renderPixel : Bool -> String
renderPixel value =
    case value of
        False -> "."
        True -> "#"

renderLine: Array Bool -> String
renderLine displayData  = 
    foldr (\a -> append (renderPixel a)) "" displayData
       
    
renderDisplay : Display -> List String
renderDisplay display = 
    map renderLine display 
         |> Array.toList

rect : Int -> Int -> Display -> Display
rect x y displayData = 
    indexedMap (\a b -> 
        if a < y then 
            indexedMap (\c d ->
                if c < x then 
                    True
                else 
                    d) b
        else
            b) displayData

shiftedPos: Int -> Int -> Int -> Int
shiftedPos val shift length = 
    (val - ( shift - length )) % length

rotateCol: Int -> Int -> Display -> Display
rotateCol: col shift displayData = 
    let 
        actualShift = shift % length displayData
    
        clac
main = 
    div []  ( textListToDiv (renderDisplay (rect 3 2 display)) )
