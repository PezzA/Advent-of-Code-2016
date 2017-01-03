module Update exposing (..)

import Model exposing (..)
import Puzzles.Day01.Update as Day1Update exposing (..)


update : AppMsg -> AppModel -> ( AppModel, Cmd AppMsg )
update msg model =
    case msg of
        SetDay newDay ->
            ( { model | currentDay = newDay }, Cmd.none )

        Day1Message subMsg ->
            let
                ( updatedModel, updatedCmd ) =
                    Day1Update.update subMsg model.day1Model
            in
                ( { model | day1Model = updatedModel }, Cmd.map Day1Message updatedCmd )

        NoOp ->
            ( model, Cmd.none )
