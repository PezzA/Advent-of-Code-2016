module Day9 exposing (..)

import String exposing (length, toInt, slice)
import Array exposing (fromList, get)
import Regex exposing (regex, find)

import Data.Day9 exposing (..)
import Common exposing (..)


type alias Marker = 
    { prefix: String,
      command: (Int, String),
      remainder: String,
      out : String,
      outLength: Int
    }


nextMarker : Regex.Regex
nextMarker =
    regex "[(]([0-9]+)x([0-9]+)[)]"


getPart: String -> Regex.Match -> Marker
getPart dataInput match = 
    let
        matchLength = String.length match.match
        
        matchBits = Array.fromList match.submatches
        
        toTake = arrayBitToInt 0 matchBits
    in
        Marker
            (String.slice 0 match.index dataInput)
            ( arrayBitToInt 1 matchBits , String.slice (match.index + matchLength) (match.index + matchLength + toTake) dataInput )
            (String.slice (match.index + matchLength + toTake) (String.length dataInput) dataInput)
            ((String.slice 0 match.index dataInput) ++ (String.repeat (arrayBitToInt 1 matchBits) (String.slice (match.index + matchLength) (match.index + matchLength + toTake) dataInput)))
            ((arrayBitToInt 1 matchBits) * (String.length (String.slice (match.index + matchLength) (match.index + matchLength + toTake) dataInput)))
             
            


getParts: String -> List Marker
getParts data =
    let
        next = Regex.find (Regex.AtMost 1) nextMarker data
    in
        if not (List.isEmpty next) then
            List.map
                (\a -> getPart data a)
                next
        else
            [ Marker data (0,"") "" "" (String.length data) ]
        
   
getAllParts data =
    let 
        newData = getParts data
        
        remain = 
            case List.head (List.reverse newData) of
                Just val -> val.remainder
                Nothing -> ""
    in 
        if not (String.isEmpty remain) then
            List.append (getAllParts remain) newData
        else
            newData
    
doPartOne data = 
    ( String.concat (List.map (\a -> a.out) (getAllParts data)), List.foldr (\a b -> a.outLength + b ) 0 (getAllParts data))
    
    