module Day10 exposing (..)

import Regex exposing (regex, Regex, HowMany, find, Match)
import Array exposing (slice, fromList, Array, get)
import String exposing (toInt)
import Maybe exposing (withDefault)

import Data.Day10 exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg
    = NextStep
    | NoOp

type Status
    = Dormant
    | Engaged
    | Active
    | Target
    | Deactivated


type alias OpBot =
    { id : Int
    , input1 : Int
    , input2 : Int
    , lowTo : Recipient
    , highTo : Recipient
    , status : Status
    }

type alias Model =
    { instructions: List Instruction
    , inputs : List (Int, Int)
    , bots : List OpBot
    , outputs : List (Int, Int)
    , complete : Bool
    }

init : ( Model, Cmd Msg )
init =
    ( processInstructions day10PuzzleInput, Cmd.none )


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
        NextStep ->
            ( process model, Cmd.none)
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

rectHelper : Int -> Int -> Int -> Int -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
rectHelper iX iY iWidth iHeight styles =
    [ x (toString iX)
    , y (toString iY)
    , width (toString iWidth)
    , height (toString iHeight)
    ]
        ++ styles

drawInput : Int -> Int -> (Int, Int) -> Svg msg
drawInput ix iy (index, input) =
    let
        textTransform = "translate (" ++ (toString ix) ++ ", " ++ (toString iy) ++ ")"
    in
    g [ transform textTransform ]
        [ rect (rectHelper 0 0 50 50 [ fill "#00FF00", stroke "#000000" ]) []
        , rect (rectHelper 0 0 10 10 [ fill "#0000FF", stroke "#000000" ]) []
        , Svg.text_ [ x "6", y "6", stroke "#FFFFFF", textAnchor "middle", alignmentBaseline "central", fontSize "12", fontFamily "arial"] [ Svg.text (toString input) ]
        , Svg.text_ [ x "25", y "25", stroke "#000000", textAnchor "middle", alignmentBaseline "central", fontSize "36", fontFamily "arial"] [ Svg.text (toString index) ]
        , circle [ cx "25", cy "50", r "5", fill "#ff0000"] []
        ]

drawBots : Int -> Int -> OpBot -> Svg msg
drawBots ix iy ibot =
    let
        textTransform = "translate (" ++ (toString ix) ++ ", " ++ (toString iy) ++ ")"

        style = case ibot.status of
            Dormant ->
                [ fill "#777777", stroke "#000000"]
            Engaged ->
                [ fill "#FFFF00", stroke "#000000"]
            Active ->
                [ fill "#00FF00", stroke "#000000"]
            Deactivated ->
                [ fill "#222222", stroke "#000000"]
            Target ->
                [ fill "#FF0000", stroke "#000000"]
    in
        g [ transform textTransform, textAnchor "middle", alignmentBaseline "central", fontFamily "arial", fontSize "12" ]
            [ rect (rectHelper 0 0 50 50 style) []
            , rect (rectHelper 0 0 20 20 style) []
            , rect (rectHelper 30 0 20 20 style) []
            , rect (rectHelper 0 30 20 20 style) []
            , rect (rectHelper 30 30 20 20 style) []
            , Svg.text_ [ x "25", y "30", stroke "#000000" ] [ Svg.text (toString ibot.id) ]
            , Svg.text_ [ x "10", y "15", stroke "#000000" ] [ Svg.text (toString ibot.input1) ]
            , Svg.text_ [ x "40", y "15", stroke "#000000" ] [ Svg.text (toString ibot.input2) ]
            , Svg.text_ [ x "10", y "45", stroke "#000000" ] [ Svg.text (
                case ibot.lowTo of
                    Output val ->
                        "0" ++ toString val
                    Bot val ->
                        "B" ++ toString val
                    _ -> "") ]
            , Svg.text_ [ x "40", y "45", stroke "#000000" ] [ Svg.text (
                case ibot.highTo of
                    Output val ->
                        "0" ++ toString val
                    Bot val ->
                        "B" ++ toString val
                    _ -> "") ]
            ]

drawOutput : Int -> Int -> (Int, Int) -> Svg msg
drawOutput ix iy (index, input) =
    let
        textTransform = "translate (" ++ (toString ix) ++ ", " ++ (toString iy) ++ ")"
    in
    g [ transform textTransform ]
        [ rect (rectHelper 0 0 50 50 [ fill "#0000FF", stroke "#000000" ]) []
        , Svg.text_ [ x "6", y "6", stroke "#FFFFFF", textAnchor "middle", alignmentBaseline "central", fontSize "12", fontFamily "arial"] [ Svg.text (toString index) ]
        , Svg.text_ [ x "25", y "25", stroke "#FFFF00", fill "#FFFF00", textAnchor "middle", alignmentBaseline "central", fontSize "36", fontFamily "arial"] [ Svg.text (toString input) ]
        , circle [ cx "25", cy "0", r "5", fill "#ff0000"] []
        ]


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "5000", height "800", viewBox ("0 0 5000 800"), shapeRendering "optimizeSpeed" ]
            ( (List.indexedMap (\ index a -> drawInput ((60 * index)+20) 20 a) (model.inputs))
            ++ (List.indexedMap (\ index a -> drawBots ((60 * index)+20) 200 a) (List.filter (\a -> a.status == Dormant) model.bots)) -- dormant
            ++ (List.indexedMap (\ index a -> drawBots ((60 * index)+20) 100 a) (List.filter (\a -> a.status == Engaged || a.status == Active) model.bots))  -- enagaged and active
            ++ (List.indexedMap (\ index a -> drawBots ((60 * index)+20) 400 a) (List.filter (\a -> a.status == Deactivated) model.bots))  -- deactivated
            ++ (List.indexedMap (\ index a -> drawBots ((60 * index)+20) 500 a) (List.filter (\a -> a.status == Target) model.bots))  -- The Answer to part 1!!!
            ++ (List.indexedMap (\ index a -> drawOutput ((60 * index)+20) 600 a) (model.outputs))
            )
        , div[]
            [ button [ onClick NextStep ] [ Html.text "continue" ]
            ]
        ]



type Recipient
    = Bot Int
    | Output Int
    | NoRecipient


type Instruction
    = Value Int Recipient
    | WireBot Int Recipient Recipient
    | NoInstruction


commandRegex : Regex
commandRegex =
    Regex.regex "(value|bot|output) ([0-9]+) (goes to|gives low to) (bot|output) ([0-9]+)(( and high to )(bot|output) ([0-9]+))?"


getSubMatch : Array (Maybe String) -> Int -> String
getSubMatch subMatchArray index =
    case (Array.get index subMatchArray) of
        Just val ->
            Maybe.withDefault "" val

        Nothing ->
            ""


getInputs : List Instruction -> List (Int, Int)
getInputs instructions =
    List.filter
        (\a ->
            case a of
                Value int recipient ->
                    True

                _ ->
                    False
        )
        instructions
        |> List.map (\ a -> case a of
            Value val r ->
                case r of
                    Bot bIndex ->
                        (val, bIndex)
                    _ -> (-1,-1)
            _ -> (-1,-1))


-- big assumption made here that each participating bot will have it's own instruction
getBots : List Instruction -> List OpBot
getBots ins =
    List.filter
        (\a ->
            case a of
                WireBot int r1 r2 ->
                    True
                _ ->
                    False)
        ins
            |> List.map (\a -> case a of
                                   WireBot val r1 r2 ->
                                           OpBot val 0 0 r1 r2 Dormant
                                   _ -> OpBot -1 0 0 (Bot -1) (Bot -1) Dormant )




getOutputs : List Instruction -> List (Int, Int)
getOutputs instructions =
    List.foldr
        (\instruction b ->
            case instruction of
                Value int recipient ->
                    case recipient of
                        Output register ->
                            if List.member register b then
                                b
                            else
                                register :: b

                        _ ->
                            b

                WireBot val r1 r2 ->
                    List.foldr
                        (\a b ->
                            case a of
                                Output register ->
                                    if List.member register b then
                                        b
                                    else
                                        register :: b

                                _ ->
                                    b
                        )
                        b
                        [ r1, r2 ]

                _ ->
                    b
        )
        []
        instructions
        |> List.indexedMap (\ index a -> (index, 0))



getRecipient : String -> String -> Recipient
getRecipient device index =
    let
        actualIndex =
            Result.withDefault -1 (toInt index)
    in
        case device of
            "bot" ->
                Bot actualIndex

            "output" ->
                Output actualIndex

            _ ->
                NoRecipient


parseMatch : Match -> Instruction
parseMatch match =
    let
        subMatchPartial =
            getSubMatch (Array.fromList match.submatches)
    in
        case (subMatchPartial 0) of
            "value" ->
                Value (Result.withDefault -1 (toInt (subMatchPartial 1)))
                    (getRecipient (subMatchPartial 3) (subMatchPartial 4))

            "bot" ->
                WireBot (Result.withDefault -1 (toInt (subMatchPartial 1)))
                    (getRecipient (subMatchPartial 3) (subMatchPartial 4))
                    (getRecipient (subMatchPartial 7) (subMatchPartial 8))

            _ ->
                NoInstruction

processInstructions: String -> Model
processInstructions input =
    let
        extractedInstructions =
            List.map
                parseMatch
                (find (Regex.All) commandRegex input)
     in
        Model
            extractedInstructions
            (getInputs extractedInstructions)
            (getBots extractedInstructions)
            (getOutputs extractedInstructions)
            False


process: Model -> Model
process model =
        if List.isEmpty model.inputs then
            let
                nextActiveBot = case List.head ( List.filter (\a -> a.status == Active) model.bots) of
                    Just val -> val
                    Nothing -> OpBot -1 0 0 (Bot -1) (Bot -1) Dormant

                (highVal, lowVal) =
                    if nextActiveBot.input1 > nextActiveBot.input2 then
                        (nextActiveBot.input1, nextActiveBot.input2)
                    else
                        (nextActiveBot.input2, nextActiveBot.input1)

                lowToBot =
                    case nextActiveBot.lowTo of
                        Bot val -> val
                        _ -> -1

                highToBot =
                    case nextActiveBot.highTo of
                        Bot val -> val
                        _ -> -1

                lowToOutput =
                    case nextActiveBot.lowTo of
                        Output val -> val
                        _ -> -1

                highToOutput =
                    case nextActiveBot.highTo of
                        Output val -> val
                        _ -> -1

                updatedBots =
                    List.map
                        (\a ->
                            if a.id == nextActiveBot.id then
                                if (a.input1 == 61 || a.input1 == 17) && (a.input2 == 17 || a.input2 == 61) then
                                    { a | status = Target }
                                else
                                    { a | status = Deactivated }
                            else if a.id == lowToBot then
                                if a.input1 /= 0 then
                                    { a | input2 = lowVal, status = Active }
                                else
                                    { a | input1 = lowVal, status = Engaged }
                            else if a.id == highToBot then
                                if a.input1 /= 0 then
                                    { a | input2 = highVal, status = Active }
                                else
                                    { a | input1 = highVal, status = Engaged }
                            else
                                a
                        )
                        model.bots

                updatedOutputs =
                    List.map
                        (\ (a,b) ->
                            if a == lowToOutput then
                                ( a, lowVal )
                            else if a == highToOutput then
                                ( a, highVal)
                            else
                                (a,b)
                        )
                        model.outputs

            in
                { model | bots = updatedBots, outputs = updatedOutputs }
        else
            let
                (val, bot) = case (List.head model.inputs) of
                    Just val -> val
                    Nothing -> ( 0,0)

                updatedBots = List.map (\ a ->
                    if a.id == bot then
                        if a.input1 /= 0 then
                            { a | input2 = val, status = Active }
                        else
                            { a | input1 = val, status = Engaged }
                    else
                        a
                 )  model.bots
            in
                { model | inputs = List.drop 1 model.inputs, bots = updatedBots }

