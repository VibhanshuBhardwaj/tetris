module Tetromino exposing (..)
import Element exposing (show, Element)
import Basics exposing (..)
import Collage exposing (..)
import Color exposing (Color)
import Html
import List
import Block exposing (Block)
import Random exposing (Generator)
import Tuple

type alias Location  = (Int, Int)
type alias Tetromino = { shape: List Location,
                         block: Block,
                         pivot: { r : Float, c: Float},
                         rows: Int,
                         cols: Int
                        }

toForm: Tetromino -> Form 
toForm {shape, block} = 
    let form = Block.toForm block
        translate (row, col) = move ((toFloat col) * Block.size,
                                      (toFloat row) * Block.size ) form 
        forms = List.map translate shape
    in group forms     


i: Tetromino
i = { shape = [(1, 0), (0,0), (-1,0), (-2,0)], 
        block = Block Color.lightBlue, {-change to pink-}
        pivot = { r = -0.5, c = 0.5},
        rows = 4,
        cols = 1
    }

j: Tetromino
j = { shape = [(1,0), (0, 0), (-1, -1), (-1, 0)], 
        block = Block Color.blue,
        pivot = { r = 0.0, c = 0.0},
        rows = 3,
        cols = 2
    }
l: Tetromino
l = { shape = [(1,0), (0, 0), (-1, 0), (-1, 1)], 
        block = Block Color.orange,
        pivot = { r = 0.0, c = 0.0},
        rows = 3,
        cols = 2
    }
z: Tetromino
z = { shape = [(1,-1), (1, 0), (0, 0), (0, 1)], 
        block = Block Color.red,
        pivot = { r = 0.0, c = 0.0},
        rows = 2,
        cols = 3
    }
s: Tetromino
s = { shape = [(0,0), (0, 1), (-1, -1), (-1, 0)], 
        block = Block Color.green,
        pivot = { r = 0.0, c = 0.0},
        rows = 2,
        cols = 3
    }
t: Tetromino
t = { shape = [(0,-1), (0, 0), (0, 1), (-1, 0)], 
        block = Block Color.purple,
        pivot = { r = 0.0, c = 0.0},
        rows = 2,
        cols = 3
    }
o: Tetromino
o = { shape = [(0,0), (0, 1), (-1, 0), (-1, 1)], 
        block = Block Color.yellow,
        pivot = { r = -0.5, c = 0.5},
        rows = 2,
        cols = 3
    }

drawPivot: Tetromino -> Form

drawPivot {pivot} = 
    let piv = circle 5 |> filled Color.red
        translate = move (pivot.c * Block.size, pivot.r * Block.size)
    in translate piv

rotateLocation : {r: Float, c: Float} -> Float -> Location -> Location
rotateLocation pivot angle (row, col) = 
    let original_r = (toFloat row) - pivot.r
        original_c = (toFloat col) - pivot.c 
        (s, c) = (sin(angle), cos(angle))
        new_r = original_r * c - original_c * s
        new_c = original_r * s + original_c * c 
    in (round <| new_r + pivot.r, round <| new_c + pivot.c)

rotateTetromino: Tetromino -> Tetromino

rotateTetromino tetromino = 
    let specificRotationLocation = rotateLocation tetromino.pivot (degrees 90)
        newShape = List.map specificRotationLocation tetromino.shape
    in { tetromino | shape = newShape , rows = tetromino.cols, cols = tetromino.rows}


shift : Location -> Tetromino -> Tetromino
shift (rows, cols) tetromino = 
    let 
        shiftLocation (r, c) = (r + rows, c + cols)
        newShape = List.map shiftLocation tetromino.shape
        newPivot = {r = tetromino.pivot.r + (toFloat rows), c = tetromino.pivot.c + (toFloat cols)}
    in {tetromino | shape = newShape, pivot = newPivot}

zeroToOne: Generator Float 
zeroToOne =  Random.float 0 1
tetro =  shift (-9, 2) <|rotateTetromino t
shuffleBag: List Float -> List Tetromino
shuffleBag weights = 
    let tetrominos = [i, o, j, l, z, s, t]
        weighted = List.map2 (,) weights tetrominos
        sorted = List.sortBy Tuple.first weighted
    in List.map Tuple.second sorted
bag: Generator (List Tetromino)
bag = 
    let weights = Random.list 7 zeroToOne
    in Random.map shuffleBag weights
main: Html.Html msg
main =  Element.toHtml <|collage 1000 1000 [toForm tetro, drawPivot tetro]

