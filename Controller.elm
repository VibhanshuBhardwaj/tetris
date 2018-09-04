module Controller exposing (..)
import Tetromino exposing (..)
import Html exposing (Html, div, text, program)
import Collage exposing (..)
import Element
import Mouse
import Keyboard
import Char
import Time exposing (Time, second)
import Board exposing (Board)
type Input = Rotate | Shift (Int, Int)


-- MODEL


type alias Model =
    {falling: Tetromino, board: Board }

defaultModel: Model
defaultModel = {falling= Tetromino.shift startingShift Tetromino.j, board = Board.new []}
init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )



-- MESSAGES


type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time



-- VIEW
tryKicks: List (Int, Int) -> Model -> Model->Model
tryKicks shifts curr next = 
    case shifts of
        [] -> curr
        (s:: rest) ->
            let shifted = Tetromino.shift s next.falling
            in if Board.isValid shifted next.board then {next | falling = shifted}
                else tryKicks rest curr next
wallKick: Model -> Model -> Model
wallKick curr next = 
    let range = next.falling.cols // 2
        shifts = List.range 1 range |> List.concatMap (\n -> [(0,n), (0, -n)])
    in tryKicks shifts curr next
view : Model -> Html Msg
view model =
    let width = 600
        height = 600
        fallingForm = Tetromino.toForm model.falling
        boardForm = Board.addTetromino model.falling model.board |> Board.toForm
    in Element.toHtml <| collage width height [boardForm]




-- UPDATE
floorKick curr next = 
    let range = next.falling.rows //2
        shifts = List.range 1 range |> List.map (\n -> (n,0))
    in tryKicks shifts curr next

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let isUpdateValid = useIfValid model
    in case msg of 
        KeyMsg code ->
            let newFalling = arrowsToInput code model.falling
                rotated = {model | falling =  Tetromino.rotateTetromino model.falling}
                nextState = isUpdateValid rotated
                newModel = 
                    if nextState == model then wallKick model rotated else nextState
                newNewModel = 
                    if newModel == model then floorKick model rotated else newModel
                normalShift = isUpdateValid { model | falling = newFalling }
                correctMove = if (code == 38) then (newNewModel, Cmd.none) else (normalShift, Cmd.none)
            
            in correctMove
        Tick newTime ->
            let newFalling = shift (-1, 0) model.falling
                newModel = isUpdateValid {model | falling = newFalling}
            in (newModel, Cmd.none)


useIfValid : Model -> Model ->  Model 
useIfValid curr new =
    if Board.isValid new.falling new.board then new 
    else curr

-- SUBSCRIPTIONS

startingShift: (Int, Int)
startingShift = (20, 5)
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [
         Time.every second Tick,
         Keyboard.downs KeyMsg
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-
up=38, 
down=40,
left=37
right=39
-}

arrowsToInput : Int -> Tetromino -> Tetromino 

arrowsToInput keyCode t =
    if (keyCode == 38) then 
        rotateTetromino t
    else if (keyCode ==40) then 
        shift (-1, 0) t
    else if (keyCode == 37) then 
        shift (0, -1) t
    else if (keyCode == 39) then 
        shift (0, 1) t
    else 
        shift (0, 0) t

