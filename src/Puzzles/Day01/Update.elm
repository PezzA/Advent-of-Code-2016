module Puzzles.Day01.Update exposing (..)

import Puzzles.Day01.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Back ->
            let
                newModel =
                    updatePaths model -1
            in
                ( newModel, Cmd.none )

        Forward ->
            let
                newModel =
                    updatePaths model 1
            in
                ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )
