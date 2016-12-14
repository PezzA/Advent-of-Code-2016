module Day10 exposing (..)

import Array exposing (..)
type Section 
    = Wall
    | Space
    | Unknown
    

divideByTwo: Int -> (Int, Int)
divideByTwo input =
    (input // 2, input % 2 )
        
        
getRemainders: Int -> List Int
getRemainders input =
    let
        val = divideByTwo input
    in
        if fst val /= 0 then
           List.append [ snd val ] (getRemainders (fst val) )
        else
            [ snd val ]
            

getSetBitCount: Int -> Int
getSetBitCount input =
    List.filter (\a -> a == 1) (getRemainders input)
        |> List.length


isWall (x, y) salt =
    let
     
        base = (x*x) + (3*x) + (2*x*y) + y + (y*y) + salt
        
        entity = 
            case (getSetBitCount base) % 2 of   
            0 -> Space
            1 -> Wall
            _ -> Unknown
        
        _ = Debug.log "wall" (x, y, entity) 
    in   
        entity
            
            
maze x y salt = 
    Array.map (\ a -> Array.map (\ b -> isWall (b, a) salt ) (Array.fromList [0..x])) (Array.fromList [0..y])
    
    
consoleDisplay maze = 
    Array.map  (\ a -> Array.map (\b -> toString ) a) maze