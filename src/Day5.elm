module Day5 exposing (main)

import Libs.MD5 exposing (hex)
import Html exposing (text, div)
import String exposing (left)
import Debug


getValue: String -> String
getValue test = 
    hex test


testBatch: String -> Int -> Int -> List String
testBatch prefix batch index = 
    let
        start = index * batch
        end = ((index + 1) * batch) - 1
        
        -- Decrypt animation in console?
        _ = Debug.log "iteration" index 
    in
        List.map (\a -> getValue (prefix ++ (toString a))) [start..end]
            |> List.filter (\a -> String.left 5 a == "00000" )


testAllBatches: String -> Int -> List Int -> List String
testAllBatches prefix batch indicies = 
    List.map (\a -> testBatch prefix batch a ) indicies
        |> List.concat 


main = 
    div []
        (List.map (\ a -> div [] [ text a ]) (testAllBatches "ffykfhsq" 1000 [19990..29990]))
        
{- 
SPOILERS!!
10-20m
00000e1da9c24b1aab3a54427e4881e0
00000bc7014bff91acabdaa04c2e36b4
00000ae3448eda0cf4b03e477d7bb14f
00000fe244eb868fbdfb8669c0839638
00000729b28151d5bae574bee4cf6953
00000f71f44480ec3af1560ac7f6841e
00000a7325cc6dab6ca39ad3bccdd46b
000005ab6ffd053eb419c0aaf02daa5c
00000e65b4ed6759bb5f2274eca5c327
00000e095bd1af53252bc7635fa7a5e8
00000583b5bc6e887cea24907e0df958
000007ba60bbd28ca93292f666fe58d8
000000acd5e9762a7106c60088550ed9

20m-30m
000006ed719910d3d2aac1c5c34309b6
00000f03c1b40ba46e7665324fde042b
000008e7cf60038c10243b00f3fd7301
00000ef5ef7334a470efea70332d9c07
0000075db939b5b10a25ef7a7e610b37
000001c3c28bcbacf0f543a33548ef24
000004d57fc545f376c09f27383b2c88
0000062ad27ec5733af76d15764d4392
0000023d12c49f028699d4679ba91780
000003a5e614e565b54aad4504361dc8
000007608f32dff20a7f67ce26d3c676
00000c5cf0fd0b2649d412e750a53d04

0000008239d1bbf480ea541e9da1e494
000001c3c28bcbacf0f543a33548ef24
0000023d12c49f028699d4679ba91780
00000351ce68ffb449644d4bfa4cee5d
000004d57fc545f376c09f27383b2c88
0000051079ac6b44fc3a5266a1630d42
000006a94bb1c9322cbb56dd8564e76e
000007b2e0e83dfeade14ebe09f9e6a7
-}