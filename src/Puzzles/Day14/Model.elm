module Puzzles.Day14.Model exposing (..)

import MD5


getValue : String -> String
getValue test =
    MD5.hex test

testSalt : String
testSalt =
    "abc"

puzzleSalt : String
puzzleSalt =
    "ahsbgdzn"


containsEndMarker

isMarker : String -> Bool
isMarker token =
    ((String.slice 0 1 token) == (String.slice 1 2 token))
    && ((String.slice 1 2 token) == (String.slice 2 3 token))

containsMarker : String -> List Mark
containsMarker input =
     let
        maxLen =
            (String.length input) - 3
     in
        List.map (\a -> isMarker (String.slice a (a + 3) input)) (List.range 0 maxLen)
            |> List.any (\a -> a)

getOneTimeKeys hashSalt index =
    let
        hash = getValue (hashSalt ++ (toString index))
    in
        if containsMarker hash then
            True
        else
            False


