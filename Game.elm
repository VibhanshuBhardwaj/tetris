module Game exposing (..)
import Tetromino exposing (..)
import Html exposing (Html, div, text, program)
import Collage exposing (..)
import Element exposing (Element, show, flow, down)
import Mouse
import Keyboard
import Char
import Time exposing (Time, second)
import Random exposing (Generator, Seed)
import Board exposing (Board)
import Tuple
import Dict
import Text


type Input = Rotate | Shift (Int, Int)

type Msg
    = KeyMsg Keyboard.KeyCode
    | Tick Time


initialSeed = 69

startingShift: (Int, Int)
startingShift = (20, 5)

init : ( Model, Cmd Msg )
init = ( defaultModel, Cmd.none )

type alias Model =
    {falling: Tetromino, board: Board, seed: Seed, bag: List Tetromino, score: Int, difficulty: Time}

defaultModel: Model
defaultModel = 
    let initSeed = Random.initialSeed initialSeed
        bag = Random.step Tetromino.bag initSeed |> Tuple.first
        seed = Random.step Tetromino.bag initSeed |> Tuple.second
        falling = List.head bag |> Maybe.withDefault Tetromino.j
        newBag = List.drop 1 bag
    in { falling= Tetromino.shift startingShift falling,
                seed = seed,
                bag = newBag,
                board = Board.new [],
                score = 0,
                difficulty = Time.second     
        }


isGameOver: Model -> Bool
isGameOver model  =
    let checkIfOver (r, c) = r>=20 
    in List.any checkIfOver (Dict.keys model.board)

checkBag: Model -> Model
checkBag state = 
    if not (List.isEmpty state.bag ) then state
    else let (bag, seed) = Random.step Tetromino.bag state.seed
        in {state | bag = bag, seed = seed}

nextTetromino : Model -> Model
nextTetromino model = 
    let state = checkBag model
        nextFalling = List.head state.bag |> Maybe.withDefault Tetromino.o |>
                        Tetromino.shift startingShift
        nextBag = List.drop 1 state.bag 
        (lines, nextBoard) = Board.addTetromino state.falling state.board |> Board.clearLines
        bonusScore = if (lines == 1) then 1 else lines * 5
    in {state | falling = nextFalling, bag=nextBag, board= nextBoard, score  =state.score + bonusScore} 


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

floorKick: Model -> Model -> Model
floorKick curr next = 
    let range = next.falling.rows //2
        shifts = List.range 1 range |> List.map (\n -> (n,0))
    in tryKicks shifts curr next


useIfValid : Model -> Model ->  Model 
useIfValid curr new =
    if Board.isValid new.falling new.board then new 
    else curr

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


view : Model -> Html Msg
view model =
    let width = 600
        height = 600
        fallingForm = Tetromino.toForm model.falling
        boardForm = Board.addTetromino model.falling model.board |> Board.toForm
        title = show <| "Quatris | Use arrow keys to move the falling pieces. The up arrow rotates the falling piece."
        messageToDisplay = if (isGameOver model) then "Game over! Your score was " ++ (toString model.score) ++ ". Press R to restart"
                            else "Current score: " ++ (toString model.score) ++ ". Press D to make the game more difficult"
        elem = flow down [ collage width height [boardForm], show <| messageToDisplay]
        completeElem = flow down [title, elem]
    in Element.toHtml <| completeElem


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
                correctMove = if (code == 38) then (newNewModel, Cmd.none) 
                                else if (code == 82) then (defaultModel, Cmd.none)
                                else if (code == 68) then ({model | difficulty = Time.millisecond * 100}, Cmd.none)
                                else (normalShift, Cmd.none)
            
            in correctMove
        Tick newTime ->
            let newFalling = shift (-1, 0) model.falling
                isValid = Board.isValid newFalling model.board
                newModel = if isValid then {model | falling = newFalling}
                            else if (isGameOver model) then {defaultModel | score = model.score }
                            else nextTetromino model
            in (newModel, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if (not (isGameOver model)) then Sub.batch[
                Time.every model.difficulty Tick,
                Keyboard.downs KeyMsg
                ]
    else Keyboard.downs KeyMsg
    --else Sub.none

-- MAIN

main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

