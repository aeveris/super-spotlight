module Objects exposing (foo)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond)

type alias Object =
    { pos : (Float, Float), ttl : Time, form : Form }

foo : Form
foo = scale 3 (filled black (polygon [(0,0), (10,-10), (0,-10), (-10,0)]))

make_object : (Float, Float) -> Form -> Object
make_object ((x, y) as pos) f = Object pos (6000 * millisecond) f
