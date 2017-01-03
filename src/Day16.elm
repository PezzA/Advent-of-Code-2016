module Day16 exposing (..)


swapAndFlip : String -> String
swapAndFlip input = 
    List.map  (\ a -> case a of
        '0' -> '1'
        _ -> '0')
        (List.reverse <| String.toList input)
        |> String.fromList


iterateData : String -> String
iterateData input = 
    input ++ "0" ++ (swapAndFlip input)


generateData state size = 
    if (String.length state) >=  size then
        String.slice 0 size state
    else 
        generateData (iterateData state) size

getBit input = 
    case input of
        "11" -> '1'
        "00" -> '1'
        _ -> '0'

reduceData input = 
    List.map
        (\ index -> getBit (String.slice ((index * 2)-2) (index * 2)  input))
        (List.range 1 ((String.length input) // 2))
            |> String.fromList