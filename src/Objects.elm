module Objects exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond, Time)


type alias Object =
    { pos : ( Float, Float ), ttl : Time, form : Form }


foo : Form
foo =
    scale 3 (filled black (polygon [ ( 0, 0 ), ( 10, -10 ), ( 0, -10 ), ( -10, 0 ) ]))


makeObject : ( Float, Float ) -> Form -> Object
makeObject (( x, y ) as pos) f =
    Object pos (6000 * millisecond) f
