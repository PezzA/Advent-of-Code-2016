module Model exposing (..)

import Puzzles.Day01.Model as Day1Model
import Data.Day01 as Day1Data exposing (..)


type alias AppModel =
    { currentDay : Int
    , day1Model : Day1Model.Model
    }


appCanvasWidth =
    800


appCanvasHeight =
    800


appCanvasGridSize =
   10


initialModel : AppModel
initialModel =
    { currentDay = 0
    , day1Model =
        Day1Model.newModel Day1Data.puzzleInput
            (Day1Model.Setup appCanvasWidth appCanvasHeight appCanvasGridSize)
    }


type AppMsg
    = SetDay Int
    | Day1Message Day1Model.Msg
    | NoOp
