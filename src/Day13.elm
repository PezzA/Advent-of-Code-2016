module Day13 exposing (..)

import Array exposing (map, fromList, toList)
import String exposing (concat)

type Section 
    = Wall
    | Space
    | Path
    | Agent
    | Exit
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
    in   
        entity
            
            
maze x y salt = 
    Array.map (\ a -> Array.map (\ b -> isWall (b, a) salt ) (Array.fromList [0..x])) (Array.fromList [0..y])


renderPixel : Section -> String
renderPixel value =
    case value of
        Space -> " "
        Wall -> "#"
        Path -> "."
        Agent -> "A"
        Exit -> "E"
        Unknown -> "/"
        
consoleDisplay x y salt = 
    let 
        newMaze = maze x y salt
       
        display = Array.map (\a -> Array.map renderPixel a
            |> toList
            |> String.concat
            |> Debug.log "") newMaze
    in 
        True
            
           
    