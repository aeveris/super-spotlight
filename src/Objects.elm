module Objects exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond, Time)


type alias Object =
    { pos : ( Float, Float ), ttl : Time, form : Form }


foo : Form
foo =
    scale 4 (filled red (rect 10 10))


makeObject : ( Float, Float ) -> Form -> Object
makeObject pos f =
    Object pos (6000 * millisecond) (move pos f)
