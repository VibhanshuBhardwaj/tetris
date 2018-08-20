module Tetromino exposing (..)
import Element exposing (show, Element)
import Basics exposing (..)
import Collage exposing (..)
import Color exposing (Color)
import Html
import List
import Block exposing (Block)

type alias Location  = (Int, Int)
type alias Tetromino = { shape: List Location
                        , block: Block
                        }

toForm: Tetromino -> Form 

toForm {shape, block} = 
    let form = Block.toForm block
        translate (row, col) = move ((toFloat col) * Block.size,
                                      (toFloat row)* Block.size ) form 
        forms = List.map translate shape
    in group forms     


i: Tetromino
i = { shape = [ (1, 0), (0,0), (-1,0), (-2,0)], block = Block Color.lightBlue}

j: Tetromino
j = {shape = [(1,0), (0, 0), (-1, -1), (-1, 0)], block = Block Color.blue}

tetro = j
main: Html.Html msg
main = Element.toHtml <|collage 400 400 [toForm tetro]

