module View exposing (..)

import Html exposing (div, text, a, Html, button, h1)
import Html.Attributes exposing (href, style)
import Html.Events exposing (onClick)
import Model exposing (..)

import Puzzles.Day01.View as Day1View exposing (..)
import Styles

view : AppModel -> Html AppMsg
view model =
    div [ style Styles.container ]
        [ h1 [ style Styles.heading ] [ text "Advent of Code 2016" ]
        , navBar model
        , (case model.currentDay of
            1 ->
                Html.map Day1Message (Day1View.view model.day1Model)

            _ ->
                text "No Day Selected"
          )
        ]


navBar : AppModel -> Html AppMsg
navBar model =
    div []
        [ button [ onClick (SetDay 0) ] [ text "Home" ]
        , text " "
        , button [ onClick (SetDay 1) ] [ text "Day 1" ]
        ]
