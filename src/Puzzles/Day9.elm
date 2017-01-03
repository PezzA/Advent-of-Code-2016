module Day9 exposing (..)

import String exposing (length, toInt, slice)
import Array exposing (fromList, get)
import Regex exposing (regex, find)
import Data.Day9 exposing (..)
import Common exposing (..)


type alias Marker =
    { --prefix: String,
      --command: (Int, String),
      remainder : String
    , --out : String,
      outLength : Int
    }


nextMarker : Regex.Regex
nextMarker =
    regex "[(]([0-9]+)x([0-9]+)[)]"


getPart : String -> Regex.Match -> List Marker
getPart dataInput match =
    let
        matchLength =
            String.length match.match

        matchOffset =
            match.index + matchLength

        matchBits =
            Array.fromList match.submatches

        repeatLength =
            arrayBitToInt 0 matchBits

        repeats =
            arrayBitToInt 1 matchBits

        repeatString =
            String.slice matchOffset (matchOffset + repeatLength) dataInput

        prefix =
            String.slice 0 match.index dataInput

        remainder =
            (String.slice (matchOffset + repeatLength) (String.length dataInput) dataInput)
    in
        if not (String.isEmpty remainder) then
            getParts remainder
        else
            [ Marker remainder (repeats * (String.length repeatString)) ]


getParts : String -> List Marker
getParts data =
    let
        next =
            Regex.find (Regex.AtMost 1) nextMarker data
    in
        if not (List.isEmpty next) then
            List.map
                (\a -> getPart data a)
                next
        else
            [ Marker "" (String.length data) ]


getAllParts data =
    let
        newData =
            getParts data

        remain =
            case List.head (List.reverse newData) of
                Just val ->
                    val.remainder

                Nothing ->
                    ""
    in
        if not (String.isEmpty remain) then
            List.append (getAllParts remain) newData
        else
            newData


doPartOne data =
    ( "", List.foldr (\a b -> a.outLength + b) 0 (getAllParts data) )
