module Controller exposing (..)
import Keyboard exposing (presses)
import Basics exposing (..)
import Element exposing (Element, show)
{-import Signal exposing (Signal)-}
import Platform.Sub exposing (..)

type Input = Rotate | Shift (Int, Int)



arrowsToInput : {x: Int, y: Int} -> Input

arrowsToInput {x, y} =
    if (y == 1) then Rotate else Shift (y, x)

inputs:

main =  Platform.Sub.map arrowsToInput presses