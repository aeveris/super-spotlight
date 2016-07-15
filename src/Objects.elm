module Objects exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond, Time)


type alias Object =
    { pos : ( Float, Float ), ttl : Time, form : Form }


foo : Form
foo = (filled black (rect 40 40))

makeObject : (Float, Float) -> Form -> Object
makeObject (x, y) f = Object (x, y) (6000 * millisecond) f



