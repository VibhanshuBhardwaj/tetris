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


view : Model -> Html Msg
view model =
    let width = 800
        height = 400
        fallingForm = Tetromino.toForm model.falling
        boardForm = Board.addTetromino model.falling model.board |> Board.toForm
    in Element.toHtml <| collage width height [boardForm]




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        KeyMsg code ->
            let newFalling = arrowsToInput code model.falling
                newModel = { model | falling = newFalling }
            in (newModel, Cmd.none)
        Tick newTime ->
            let newFalling = shift (-1, 0) model.falling
                newModel = {model | falling = newFalling}
            in (newModel, Cmd.none)


{-useIfValid : Model -> Model ->  Model     -}

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

