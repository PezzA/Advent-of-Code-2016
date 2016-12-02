module Main exposing (..)

import Html exposing (..)
import String
import Array exposing (..)

type alias TestRecord = ( String, String )
type alias KeyPad = Array (Array Int)
type alias KeyRow = Array Int
type alias Key = Int
type alias Position = ( Int, Int )

testData : List TestRecord
testData =
    [ ( "Test One", "ULL
RRDDD
LURDL
UUUUD" )
    , ( "Test Two", "RUDULRLLUULRURDDRRUDURULLLDRLRLUDDLUDUDDUDRRDUDULDUUULLRULLRLDDLDLDDRLRRRRUDLLDDUULDRLLUDDRRUURLULRRRDLLURRUUDURUDDURLUDDDLUDDUUDUURUDLRDRDRLRDRLDRUDRUUDLRDDRRURDDLRDDRRURDUDDLULLUDRURURRRLRRUDUULULULRRLDLUDUURRLLRUDLLDRDDLRRRULRUDLULDDLLLULDLRUDLLLLRDDLRDRLDRLLRDRRDLRDULULRLLLDRUDRRRUULRUULDRURLUDRURRDLLDLRDLDDDDRRLUDLRRLUUUURDRDDLRRURURRDUULLRLURLURUDDDRDURDUUDRLRLRRLDDLDLDLDDDUDDULURLDDLLRLRRDULUDDLULRLUDDLDLRULUUUDRLDRUDURLUDDRLLRUULDLRRRRDLLLLURULLRDRRUDLUULRRDLLRLRLUDLDDULLDLLRDLDLL
LLUUUUUUDUDRLRDRDLDURRRLLRRLRURLLUURRLLUDUDLULUURUUURDLUDLDDLULLRDLRUULDLRDUDURLLDDUDUDULLUDDUULLLUULRRRLULRURRDLRUDUDDURRRDRUURDURLLULLRULLDRUULLURLDRDUUDDDDDDRRLDRLRRRLULDDUURRLLLLDRURLURDRDRDURUDUURRDUDUDRLLUUDDRLUDDDRDLDLRLDRURRDLLRULDRLLURURRLUULLRLRRURDDRDRUUURUURUUUDLLRRLUDRLDLRLURLDLUDDUDDDLDUDRRLDLRURULRLLRDUULURRRULDLLLRLDDDUURRRRDULLRURRLULULDLRRUDUDDLRUURDLDUDDUDRRDLRRRDUDUUUDLLDDDDLURLURRRUUULLLULRRLLLLLLULDUUDLRUDRRDLRDUUDUDLLRLDLLRUURDUUURUUUDDLLUUDLULDURLULULUUUDRUDULLURRULRULLRDLDDU
RLUUURULLDLRLDUDRDURRDUURLLUDDDUULRRRLRLURDDRUULUDULDUUDDDDUDDDDRUDDLDUUDRUDLRRRLLRDDLLLRLLRUULRUULDDRURRLURRLRLULDDRRRDDURDDRDRDULRUDRUUDULRLLULDLRLLDRULRDDRRDDUDLRLLUDRDRRRLUDULRDLRDDURRUUDDRRUDURRUUUDDRRDUDURLUUDUDUURDDDLURLULLUULULURUDUUDRUDULLUUULURDLDUULLDDLLDULRLRLRDUUURUUDLRLDURUDRLDULLUDLDLLRDUURRDUDURLUUUDLLRRULRLULRLDLLURDURRULRLLRRDUDLLRDRRRRDLUUDRUUUDDLRLUDDDDDDRURRRUUURRDLLRURLDDLLDLRRLLLDRRULRRUDLDRDDRRLULURLLUURURURRRRUUUUURUDURLRLLLULULDLLDLRDRRULUDUDRDRRDRDRRDUDLLLRUDRUDDDULRULRRRDRLRUUUURUDURDUUULLULRUDDULDUUDLDURRD
ULRULDDLDLULLLRRRLRUDDDDDLLDDUDLRRDULUUDRDLRRURDRRLUULRURUDRRULDLLLUDRUUDULULUDDRUDDDRDURRRDRDUUURLRDULUDRDRLDRUDDLLLDRRULUDLUDLDLLRRUDUULULDLDLLUURDLDDLLUUDURLURLLLDRDLDRRLRULUURRDRULRUUURULRRUDDDDLLDLDDLLRRLRRRRDUUDUDLDRDRRURDLRURULDLRDLLLLRUDRLLRDLRLRDURDRUDURRRLRDRDLLRLUDDDDRLRLLDUURRURLUURUULUDLUURDRRUDDLUDUDDDURRDRUDRLRULDULUUUUUUDDUDRUDUUURUDRRDLUDLUUDUULUDURDLDDDLLURRURUUDUDDRRDRLLULULDRLRURRDDDRDUUURDDDRULUDRDDLDURRLDDDLRRRLDDRDURULDLUDLLLURLURRLRRULDLLDDUDRRULDRRRRLURRUULRRRUDLURDLLDLLDULUUDRRLDLLLDRLRUDLUULDLDRUDUDURDRUDRDDDLRLULLUR
LRLUUURRLRRRRRUURRLLULRLULLDLUDLUDRDDRLDLRLULLURDURLURDLLRLDUUDDURRRRLDLLRULLRLDLLUUDRLDDLLDRULDRLLRURDLRURRUDLULLRURDLURRURUDULLDRLLUUULUDRURRUUDUDULUUULRLDDULDRDLUDDUDDDLRURULLDLLLRLLUURDLRUDLLLLDLLRLRUUUDDRUUUUDLDLRDDURLDURUULLLUUDLLLLDULRRRLLDLDRRDRLUDRUDURLLUDLRLLUDUDRDDDRDLRDLRULUULDRLUDLRLDUURLRRLUDDDUUDDDUDRLDLDUDLURUULLDDDURUUULRLUDLDURUUDRDRURUDDUURDUUUDLLDLDLDURUURLLLLRURUURURULRULLRUDLRRUUUUUDRRLLRDDUURDRDRDDDUDRLURDRRRUDLLLDURDLUUDLLUDDULUUDLDUUULLDRDLRURUURRDURRDLURRRRLLUUULRDULDDLDUURRDLDLLULRRLLUDLDUDLUUL" )
    ]


keyPad : KeyPad
keyPad = 
    Array.fromList 
        [ Array.fromList [ 1, 2, 3 ]
        , Array.fromList [ 4, 5, 6 ]
        , Array.fromList [ 7, 8, 9 ]
        ]

pos : Position
pos = 
    ( 1, 1 )

getRow : KeyPad -> Int -> KeyRow
getRow pad index = 
     case Array.get index pad of
        Just val ->
            val
        Nothing ->
            Array.initialize 3 (always 0)
     


getCol : KeyRow -> Int -> Key
getCol row index =
    case Array.get index row of
        Just val ->
            val
        Nothing ->
            0  


getKey : KeyPad -> Position -> Key
getKey keyPad ( x, y ) = 
    let
        row = getRow keyPad y

        key = getCol row x
    in 
        key


move : String -> Position -> Position
move instruction (x, y)  = 
    case instruction of
        "U" -> shiftUp ( x, y )
        "D" -> shiftDown ( x, y )
        "L" -> shiftLeft ( x, y )
        "R" -> shiftRight ( x, y )
        "\"" -> ( 4 , 4 )
        "'" -> ( 6 , 6 )
        _ -> ( 5 , 5 )


shiftUp : Position -> Position
shiftUp ( x, y ) = 
    if y > 0 then
        ( x, y - 1 )
    else
        ( x, y )


shiftDown : Position -> Position
shiftDown ( x, y ) = 
    if y < 2 then
        ( x, y + 1 )
    else
        ( x, y )


shiftLeft : Position -> Position
shiftLeft ( x, y ) = 
    if x > 0 then
        ( x - 1, y  )
    else
        ( x, y )


shiftRight : Position -> Position
shiftRight ( x, y ) = 
    if x < 2 then
        ( x + 1, y  )
    else
        ( x , y )


processLine: String -> Position -> Position
processLine inputData (x, y ) = 
    let
        commands = String.toList inputData |> List.map String.fromChar 
        
        newPosition = List.foldr move ( x , y ) commands
    in
        newPosition


{-
processInstructions: List String -> Position -> Position
processInstructions lines position = 
    let 
        newPosition = List.folder processLine pos (String.lines lines)
    

-}
testRow: TestRecord -> Html msg
testRow ( name, test ) = 
    tr []
        [ td [] [ text name ]
        , td [] [ text (toString test) ]
        ]



main =
    div []
        [ h1 [] [ text "Day Two" ]
        , table [] (List.map testRow testData)
        ]
       
  