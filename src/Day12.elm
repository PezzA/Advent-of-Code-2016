module Day12 exposing (..)

import Html exposing (Html, program, div, text, h1, button)
import Html.Events exposing (onClick)
import Array exposing (get, fromList)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, millisecond)


type Msg
    = NoOp


type alias Model =
    { program : List Line
    , caret : Int
    , registers : RegisterSet
    , complete : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model getPuzzleInput 0  (RegisterSet 0 0 1 0) False, Cmd.none )

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
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


drawLine : Line -> Bool -> Svg Msg
drawLine line current =
    let
        textTransform =
            "translate (40, " ++ (toString ((line.index * 20) + 20)) ++ ")"

        currentStyle =
            if current then
                [ fill "#FF0000" ]
            else
                []
    in
        g ([ transform textTransform, textAnchor "middle", alignmentBaseline "central", fontFamily "arial", fontSize "12" ] ++ currentStyle)
            [ Svg.text_ [ x "10", y "10" ] [ Svg.text (toString line.index) ]
            , Svg.text_ [ x "150", y "10" ] [ Svg.text (toString line.command) ]
            , Svg.text_ [ x "350", y "10" ] [ Svg.text (toString line.sourceCode) ]
            ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ Html.text "Day 12" ]
        , svg [ width "800", height "400", viewBox ("0 0 800 400"), shapeRendering "optimizeSpeed" ]
            (List.map (\a -> drawLine a (a.index == model.caret)) model.program)
        , div [] [ Html.text (toString model.registers) ]
        ]


processCommand : Model -> ( RegisterSet, Int, Bool )
processCommand model =
    let
        commandToExec =
            case List.head (List.filter (\a -> a.index == model.caret) model.program) of
                Just val ->
                    val
                Nothing ->
                    Line -1 False NoCommand ""

        ( newRegSet, newIndex ) =
            case commandToExec.command of
                Inc val ->
                    ( shift val 1 model.registers, model.caret + 1 )

                Dec val ->
                    ( shift val -1 model.registers, model.caret + 1 )

                Cpy string register ->
                    ( copy string register model.registers, model.caret + 1 )

                Jnz source shift ->
                    let
                        actualSource =
                            case String.toInt source of
                                Ok val ->
                                    val

                                Err err ->
                                    case source of
                                        "a" ->
                                            model.registers.a

                                        "b" ->
                                            model.registers.b

                                        "c" ->
                                            model.registers.c

                                        "d" ->
                                            model.registers.d

                                        _ ->
                                            0
                    in
                        if actualSource == 0 then
                            ( model.registers, model.caret + 1 )
                        else
                            ( model.registers, model.caret + shift )

                _ ->
                    ( model.registers, model.caret )
    in
        if newIndex >= (List.length model.program) then
            ( newRegSet, newIndex, True )
        else
            processCommand (Model model.program newIndex newRegSet False)


type alias Line =
    { index : Int
    , caret : Bool
    , command : Command
    , sourceCode : String
    }


type Command
    = Cpy String Char
    | Inc Char
    | Dec Char
    | Jnz String Int
    | NoCommand


type Target
    = Register Char
    | Integer Int
    | Unknown


type alias RegisterSet =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


extractCommand : String -> Command
extractCommand input =
    let
        tokens =
            String.split " " input
                |> fromList
    in
        case String.left 3 input of
            "jnz" ->
                Jnz
                    (case (get 1 tokens) of
                        Just val ->
                            val

                        Nothing ->
                            ""
                    )
                    (case (get 2 tokens) of
                        Just val ->
                            case String.toInt val of
                                Ok val ->
                                    val

                                Err err ->
                                    -1

                        Nothing ->
                            -1
                    )

            "inc" ->
                case (get 1 tokens) of
                    Just val ->
                        Inc (stringToChar val)

                    Nothing ->
                        NoCommand

            "dec" ->
                case (get 1 tokens) of
                    Just val ->
                        Dec (stringToChar val)

                    Nothing ->
                        NoCommand

            "cpy" ->
                Cpy
                    (case (get 1 tokens) of
                        Just val ->
                            val

                        Nothing ->
                            ""
                    )
                    (case (get 2 tokens) of
                        Just val ->
                            stringToChar val

                        Nothing ->
                            ' '
                    )

            _ ->
                NoCommand


stringToChar : String -> Char
stringToChar input =
    case input of
        "a" ->
            'a'

        "b" ->
            'b'

        "c" ->
            'c'

        "d" ->
            'd'

        _ ->
            ' '


shift : Char -> Int -> RegisterSet -> RegisterSet
shift char modifier registerSet =
    case char of
        'a' ->
            { registerSet | a = registerSet.a + modifier }

        'b' ->
            { registerSet | b = registerSet.b + modifier }

        'c' ->
            { registerSet | c = registerSet.c + modifier }

        'd' ->
            { registerSet | d = registerSet.d + modifier }

        _ ->
            registerSet


set : Char -> Int -> RegisterSet -> RegisterSet
set char newVal registerSet =
    case char of
        'a' ->
            { registerSet | a = newVal }

        'b' ->
            { registerSet | b = newVal }

        'c' ->
            { registerSet | c = newVal }

        'd' ->
            { registerSet | d = newVal }

        _ ->
            registerSet


copy : String -> Char -> RegisterSet -> RegisterSet
copy source destination registerSet =
    let
        actualSource =
            case String.toInt source of
                Ok val ->
                    Integer val

                Err err ->
                    case source of
                        "a" ->
                            Register 'a'

                        "b" ->
                            Register 'b'

                        "c" ->
                            Register 'c'

                        "d" ->
                            Register 'd'

                        _ ->
                            Unknown
    in
        case actualSource of
            Integer val ->
                set destination val registerSet

            Register reg ->
                case reg of
                    'a' ->
                        set destination registerSet.a registerSet

                    'b' ->
                        set destination registerSet.b registerSet

                    'c' ->
                        set destination registerSet.c registerSet

                    'd' ->
                        set destination registerSet.d registerSet

                    _ ->
                        registerSet

            Unknown ->
                registerSet


getTestInput : List Line
getTestInput =
    List.indexedMap (\index a -> Line index False (extractCommand a) a) (String.lines testInput)


getPuzzleInput : List Line
getPuzzleInput =
    List.indexedMap (\index a -> Line index False (extractCommand a) a) (String.lines puzzleInput)


testInput =
    """cpy 41 a\x0D
inc a\x0D
inc a\x0D
dec a\x0D
jnz a 2\x0D
dec a"""


puzzleInput =
    """cpy 1 a\x0D
cpy 1 b\x0D
cpy 26 d\x0D
jnz c 2\x0D
jnz 1 5\x0D
cpy 7 c\x0D
inc d\x0D
dec c\x0D
jnz c -2\x0D
cpy a c\x0D
inc a\x0D
dec b\x0D
jnz b -2\x0D
cpy c b\x0D
dec d\x0D
jnz d -6\x0D
cpy 19 c\x0D
cpy 11 d\x0D
inc a\x0D
dec d\x0D
jnz d -2\x0D
dec c\x0D
jnz c -5"""



-- part 2 started 17:27
