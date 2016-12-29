module Day01.State exposing (..)
import Time exposing (Time, second)

import Html exposing (program)
import Day01.Data exposing (..)
import Day01.Model exposing (..)
import Day01.View exposing (..)


init : ( Model, Cmd Msg )
init =
    ( Model (instructions puzzleInput) 0 (Setup 1800 800 3) [([], 0)] ([], 0) False, Cmd.none )


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
        Tick newTime ->
            updatePaths model 1

        Back ->
            updatePaths model -1

        Forward ->
            updatePaths model 1

        NoOp ->
            ( model, Cmd.none )


            
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


