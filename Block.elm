module Block exposing (..)
import Element exposing (show, Element)
import Basics exposing (..)
import Collage exposing (..)
import Color exposing (Color)
import Html


type alias Block = {color: Color} 

size: Float
size = 25

toForm: Block -> Form 
toForm block = 
    let shape = square size
        border = outlined (solid Color.black) shape

    in group [filled block.color shape, border]

main: Html.Html msg
main = Element.toHtml <|collage 400 400 [toForm (Block Color.blue)]