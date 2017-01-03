module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Model exposing (..)
import View exposing (..)
import Update exposing (..)


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( AppModel, Cmd AppMsg )
init =
    ( initialModel, Cmd.none )


subscriptions : AppModel -> Sub AppMsg
subscriptions model =
    Sub.none
