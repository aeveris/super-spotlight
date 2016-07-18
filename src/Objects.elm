module Objects exposing (..)

import Collage exposing (..)
import Color exposing (..)
import Time exposing (millisecond, Time)


type alias Object =
    { pos : ( Float, Float ), ttl : Time, form : Form }


good : Form
good =
    outlined (thickenLine <| dashed black) (rect 40 40)

goodTtl : Time
goodTtl =
   6000 * millisecond

bad : Form
bad =
    outlined (thickenLine <| dotted black) (rect 40 40)

badTtl : Time
badTtl =
   8000 * millisecond

vital : Form
vital =
    outlined (thickenLine <| solid black) (rect 40 40)

vitalTtl : Time
vitalTtl =
   6000 * millisecond


thickenLine : LineStyle -> LineStyle
thickenLine =
    (\ls -> { ls | width = 2.0 })


makeObject : ( Float, Float ) -> Time -> Form -> Object
makeObject pos t f =
    Object pos t (move pos f)
