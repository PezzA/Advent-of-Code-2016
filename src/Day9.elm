module Day9 exposing (..)

import String exposing (..)
import Regex exposing (..)
import Data.Day9 exposing (..)


nextMarker : Regex
nextMarker =
    regex "[(]([0-9]+)x([0-9]+)[)]"


doTestOne data =
    List.map
        (\a -> ( slice 0 a.index data
               , a.match
               , slice (a.index + String.length a.match) (a.index + String.length a.match + 2) data
               , slice (a.index + String.length a.match + 2) (String.length data) data))
        (Regex.find (AtMost 1) nextMarker data)

