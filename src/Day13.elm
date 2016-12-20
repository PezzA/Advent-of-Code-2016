module Day13 exposing (..)

import Array exposing (map, fromList, toList, Array, get)
import String exposing (concat)
import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (..)
import Set exposing (..)


type alias Maze =
    Array (Array Section)


type Msg
    = Tick Time
    | NoOp


type alias Model =
    { baseval : Int
    , maze : Maze
    }

entry = (1, 1)
exit = (7, 4)

init : ( Model, Cmd Msg )
init =
  --( Model 0 (createMaze 35 40 1364 |> setSection  1 1 Agent |> setSection 31 39 Exit), Cmd.none )
    ( Model 0 (createMaze 40 40 1364  |> setSection 31 39 Exit), Cmd.none )

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

getSection : Int -> Int -> Maze -> Section
getSection x y display  =
     let
         col =
             case get y display of
                 Just val ->
                     val

                 Nothing ->
                     Array.empty
     in
         case get x col of
             Just val ->
                 val

             Nothing ->
                Unknown


setSection : Int -> Int -> Section -> Maze -> Maze
setSection y x section maze =
    Array.indexedMap
        (\i a ->
            if i == x then
                Array.set y section a
            else
                a
        )
        maze

renderSection : Section -> List (Svg.Attribute msg)
renderSection value =
    case value of
        Space ->
            [ fill "#FFFFFF" ]

        Wall ->
            [ fill "#444444" ]

        Path ->
            [ fill "#6666FF" ]

        Exit ->
            [ fill "#00FF00" ]

        Unknown ->
            [ fill "#FF0000" ]

drawMaze : Maze -> List (Svg Msg)
drawMaze maze =
    let
        new =
            Array.indexedMap
                (\yIndex mazeRow ->
                    Array.indexedMap
                        (\xIndex mazeCol ->
                            rect (rectHelper (xIndex * 15) (yIndex * 15) 15 15 (renderSection mazeCol)) []
                        )
                        mazeRow
                )
                maze

        halfArray =
            Array.foldr (\a b -> Array.append a b) Array.empty new
    in
        halfArray
            |> Array.toList


rectHelper : Int -> Int -> Int -> Int -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
rectHelper iX iY iWidth iHeight styles =
    [ x (toString iX)
    , y (toString iY)
    , width (toString iWidth)
    , height (toString iHeight)
    ]
        ++ styles


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text "Day 13" ]
        , svg [ width "1800", height "800", viewBox ("0 0 1800 800"), shapeRendering "optimizeSpeed" ]
            (drawMaze model.maze)
        ]



{--Puzzle processing, not UI stuff here --}

type alias Point = (Int, Int)

type Tree a
    = Empty
    | End
    | Walled
    | LevelExit
    | Stopped
    | Pathed
    | Node a (Tree a) (Tree a) (Tree a) (Tree a)

type Section
    = Wall
    | Space
    | Path
    | Exit
    | Unknown



divideByTwo : Int -> ( Int, Int )
divideByTwo input =
    ( input // 2, input % 2 )


getRemainders : Int -> List Int
getRemainders input =
    let
        val =
            divideByTwo input
    in
        if Tuple.first val /= 0 then
            List.append [ Tuple.second val ] (getRemainders (Tuple.first val))
        else
            [ Tuple.second val ]


getSetBitCount : Int -> Int
getSetBitCount input =
    List.filter (\a -> a == 1) (getRemainders input)
        |> List.length


isWall ( x, y ) salt =
    let
        base =
            (x * x) + (3 * x) + (2 * x * y) + y + (y * y) + salt

        entity =
            case (getSetBitCount base) % 2 of
                0 ->
                    Space

                1 ->
                    Wall

                _ ->
                    Unknown
    in
        entity


createMaze : Int -> Int -> Int -> Maze
createMaze x y salt =
    Array.map (\a -> Array.map (\b -> isWall ( b, a ) salt) (Array.fromList (List.range 0 x))) (Array.fromList (List.range 0 y))

doWalk maxdepth =
    let
        maze = 
            createMaze 40 40 1364 
                |> setSection 31 39 Exit
    in
        step entry maze 0 maxdepth


checkStep maze depth maxdepth x y = 
    case getSection x y maze of
        Exit -> LevelExit
        Wall -> Walled
        Path -> Pathed
        Space -> 
            step (x, y) maze (depth) maxdepth
        _ -> End
        
step (x, y ) maze depth maxdepth =
    if depth == maxdepth then
        Stopped
    else
        case getSection x y maze of
            Exit -> LevelExit
            Path -> Pathed
            Space ->
                let 
                    newMaze = 
                        setSection x y Path maze
                    
                    mazeStepper = 
                        checkStep newMaze (depth + 1) maxdepth
                in
                    Node (x,y)
                        (mazeStepper x (y-1))
                        (mazeStepper (x+1) y)
                        (mazeStepper x (y+1))
                        (mazeStepper (x-1) y)
            _ -> End


isExit tree = 
    case tree of 
        LevelExit -> True
        _ -> False
            
hasPath tree = 
    case tree of 
        Node (x, y) n e s w ->
            True
        _ -> False
        
exitFound tree = 
    case tree of
        Node (x, y) n e s w ->
            if isExit n || isExit e || isExit s || isExit w then
                True
            else if hasPath n || hasPath e || hasPath s || hasPath w then
                exitFound n || exitFound e || exitFound s || exitFound w
            else
                False
        LevelExit -> True
        _ -> False
        
        
flatten : Tree a -> List a
flatten tree = 
  case tree of
    Node point n s e w  -> point :: (flatten n) ++ (flatten s) ++ (flatten e) ++ (flatten w)
    _ -> []
    
-- day 2 answer : List.foldr (\a b -> if not (Set.member a b) then Set.insert a b else b) Set.empty ( flatten <| doWalk 50 ) |> Set.size
-- had to add 1 as i dont actually return the end co-ords    
