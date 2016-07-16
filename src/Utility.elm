module Utility exposing (..)

import Mouse exposing (Position)
import Json.Decode exposing (Decoder, (:=))
import Random
import List exposing (map, any, head, tail, filter)
import Objects exposing (..)


-- CANVAS DIMENSIONS


width =
    1000


height =
    800


(=>) =
    (,)


px : Int -> String
px s =
    toString s ++ "px"


posToFloat : Position -> ( Float, Float )
posToFloat p =
    ( toFloat <| p.x, toFloat <| -p.y )


correctOffset : ( Float, Float ) -> ( Float, Float )
correctOffset =
    (\( x, y ) -> ( x - width / 2, y + height / 2 ))


offsetPosition : Decoder Position
offsetPosition =
    Json.Decode.object2 Position
        ("offsetX" := Json.Decode.int)
        ("offsetY" := Json.Decode.int)


randPos : Random.Generator ( Float, Float )
randPos =
    Random.pair (Random.float (-width / 2 + 25) (width / 2 - 25)) (Random.float (-height / 2 + 25) (height / 2 - 50))


distance : ( Float, Float ) -> ( Float, Float ) -> Float
distance ( x1, y1 ) ( x2, y2 ) =
    sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)


objDist : ( Float, Float ) -> Object -> Float
objDist ( x, y ) obj =
    distance ( x, y ) obj.pos


rightDist : Float -> Bool
rightDist a =
    if a < 25 then
        True
    else
        False


listDist : ( Float, Float ) -> List Object -> List Float
listDist pos obj =
    case head obj of
        Nothing ->
            []

        Just o ->
            case tail obj of
                Just t ->
                    [ objDist pos o ] ++ listDist pos t

                Nothing ->
                    []
