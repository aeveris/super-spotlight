module Objects exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond, Time)


type alias Object =
    { pos : ( Float, Float ), ttl : Time, form : Form }


foo : Form
foo = scale 4 (filled black (rect 10 10))

makeObject : (Float, Float) -> Form -> Object
makeObject (x, y) f = Object (x-20, y-20) (6000 * millisecond) f



