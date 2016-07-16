module Objects exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond, Time)


type alias Object =
    { pos : ( Float, Float ), ttl : Time, form : Form }


good : Form
good =
    outlined (thickenLine <| solid black) (rect 40 40)


bad : Form
bad =
    outlined (thickenLine <| dashed red) (rect 40 40)


thickenLine : LineStyle -> LineStyle
thickenLine =
    (\ls -> { ls | width = 2.0 })


makeObject : ( Float, Float ) -> Form -> Object
makeObject pos f =
    Object pos (6000 * millisecond) (move pos f)
