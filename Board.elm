module Board exposing (..)
import Block exposing (Block)
import Color
import Dict exposing (Dict)

import Html exposing (Html, div, text, program)
import Collage exposing (..)
import Element exposing (Element, flow, down, show)
import Tetromino exposing (Tetromino, Location)
import Tuple 

type alias Board = Dict Location Block

new: List (Location, Block) -> Board
new = Dict.fromList

cols: Int
cols = 10
rows: Int
rows = 20
background: Form 
background = 
    let shape = rect ((toFloat cols) * Block.size) ((toFloat rows) * Block.size)
    in filled Color.black shape

addBlock: Location -> Block -> Form -> Form

addBlock (row, col) block form =
    let
        offSetX = -(toFloat (cols - 1))/2 * Block.size
        offSetY = -(toFloat (rows - 1))/2 * Block.size
        x = (toFloat col) * Block.size
        y = (toFloat row) * Block.size
        blockForm = Block.toForm block |> move (x+ offSetX, y + offSetY)
            
    in group [form, blockForm]

toForm: Board -> Form 
toForm board = Dict.foldr addBlock background board



cumSum: List Int -> List Int
cumSum = List.scanl (+) 0

iota: Int -> List Int
iota n = List.repeat (n-1) 1 |> cumSum

fillRow: Int -> Block -> Board -> Board 
fillRow row block board = 
    let columns = iota cols
        rows = List.repeat cols row 
        locations = List.map2 (,) rows columns
        blocks = List.repeat cols block
        filledRow = List.map2 (,) locations blocks |> new
    in Dict.union filledRow board


addTetromino: Tetromino -> Board -> Board 
addTetromino {shape, block} board = 
    let asBoard = List.map2 (,) shape (List.repeat 4 block) |> new
    in Dict.union asBoard board

tetromino = Tetromino.shift (0, 5) Tetromino.j
checkRow: Int -> Board -> Bool
checkRow row board =
    let blocks = Dict.filter (\ (r, _) _ -> r == row) board
    in Dict.size blocks == cols
clearRow: Int -> Board -> Board 
clearRow row board = 
    let shift(r, c) block newBoard  =
        if (r< row) then (Dict.insert (r,c) block newBoard)
        else if (r > row) then (Dict.insert (r-1, c) block newBoard)
        else newBoard
    in Dict.foldr shift Dict.empty board

clearLines = 
    let clearRec row lines board = 
        if (row > rows) then (lines, board)
        else if (checkRow row board) then clearRec row (lines + 1) (clearRow row board)
        else clearRec (row + 1) lines board
    in clearRec 0 0
testForm: Form 
testForm = addBlock (0, 0) (Block Color.blue) background

testBoard: Board 
testBoard = new [((0, 0), Block Color.blue), ((0,1), Block Color.yellow)]
test = new []
    
           
inBounds : Tetromino -> Bool
inBounds {shape} = 
    let checkLocation (r, c) = r>=0 && c>=0 && c< cols
    in List.all checkLocation shape

isIntersecting: Tetromino -> Board -> Bool
isIntersecting {shape} board = 
    let checkLocation location = Dict.member location board
    in List.any checkLocation shape {-(inBounds tetromino) && not (isIntersecting tetromino board)-}

isValid: Tetromino -> Board -> Bool
isValid tetro board = 
    (inBounds tetro) && not (isIntersecting tetro board)

main: Html.Html msg


main = Element.toHtml <| flow down [collage 600 600 [toForm (addTetromino tetromino test)], show <| isValid tetromino test]
            