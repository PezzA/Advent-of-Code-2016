import Html exposing (..)
import String
import Array exposing (..)


testOne =
    "R2, L3"


testTwo =
    "R2, R2, R2"


testThree =
    "R5, L5, R5, R3"


puzzleInput =
    "R1, R1, R3, R1, R1, L2, R5, L2, R5, R1, R4, L2, R3, L3, R4, L5, R4, R4, R1, L5, L4, R5, R3, L1, R4, R3, L2, L1, R3, L4, R3, L2, R5, R190, R3, R5, L5, L1, R54, L3, L4, L1, R4, R1, R3, L1, L1, R2, L2, R2, R5, L3, R4, R76, L3, R4, R191, R5, R5, L5, L4, L5, L3, R1, R3, R2, L2, L2, L4, L5, L4, R5, R4, R4, R2, R3, R4, L3, L2, R5, R3, L2, L1, R2, L3, R2, L1, L1, R1, L3, R5, L5, L1, L2, R5, R3, L3, R3, R5, R2, R5, R5, L5, L5, R2, L3, L5, L2, L1, R2, R2, L2, R2, L3, L2, R3, L5, R4, L4, L5, R3, L4, R1, R3, R2, R4, L2, L3, R2, L5, R5, R4, L2, R4, L1, L3, L1, L3, R1, R2, R1, L5, R5, R3, L3, L3, L2, R4, R2, L5, L1, L1, L5, L4, L1, L1, R1"



-- START ELM ARC BOILERPLATE -----------------


type Msg
    = NoOp


type alias Model =
    String


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


init : ( Model, Cmd Msg )
init =
    ( puzzleInput, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    text (toString (runCommands puzzleInput))



-- END ELM ARC BOILERPLATE -----------------


instructions : String -> Array String
instructions input =
    String.split "," input
        |> List.map String.trim
        |> Array.fromList


turnLeft : Int -> Int
turnLeft orientation =
    case orientation of
        0 ->
            270

        90 ->
            0

        180 ->
            90

        270 ->
            180

        _ ->
            0


turnRight : Int -> Int
turnRight orientation =
    case orientation of
        0 ->
            90

        90 ->
            180

        180 ->
            270

        270 ->
            0

        _ ->
            0


orientate : String -> Int -> Int
orientate direction orientation =
    case direction of
        "L" ->
            turnLeft orientation

        "R" ->
            turnRight orientation

        _ ->
            0


translate : String -> Int -> Int -> ( Int, Int ) -> ( Int, Int )
translate direction orientation shift ( x, y ) =
    case direction of
        "L" ->
            translateLeft orientation shift ( x, y )

        "R" ->
            translateRight orientation shift ( x, y )

        _ ->
            ( 0, 0 )


translateLeft : Int -> Int -> ( Int, Int ) -> ( Int, Int )
translateLeft orientation shift ( x, y ) =
    case orientation of
        0 ->
            ( x, y - shift )

        90 ->
            ( x - 1, y )

        180 ->
            ( x, y + shift )

        270 ->
            ( x + shift, y )

        _ ->
            ( 0, 0 )


translateRight : Int -> Int -> ( Int, Int ) -> ( Int, Int )
translateRight orientation shift ( x, y ) =
    case orientation of
        0 ->
            ( x, y + shift )

        90 ->
            ( x + shift, y )

        180 ->
            ( x, y - shift )

        270 ->
            ( x - shift, y )

        _ ->
            ( 0, 0 )


parseInstruction : String -> ( String, Int )
parseInstruction input =
    let
        direction =
            String.left 1 input

        shift =
            case String.toInt (String.dropLeft 1 input) of
                Ok val ->
                    val

                Err msg ->
                    0
    in
        ( direction, shift )


move : String -> ( Int, Int, Int ) -> ( Int, Int, Int )
move instruction ( x, y, orientation ) =
    let
        ( direction, shift ) =
            parseInstruction instruction

        newOrientation =
            orientate direction orientation

        newlist =
            initialize shift identity

        ( newX, newY ) =
            Array.foldr (\a ( b, c ) -> translate direction newOrientation 1 ( b, c )) ( x, y ) newlist
    in
        ( newX, newY, newOrientation )


getBlocks : ( Int, Int, Int ) -> Float
getBlocks ( x, y, orientation ) =
    (abs (toFloat x)) + (abs (toFloat y))


runCommands : String -> Float
runCommands input =
    getBlocks (Array.foldl move ( 0, 0, 0 ) (instructions input))
