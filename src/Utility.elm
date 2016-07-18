module Utility exposing (..)

import Mouse exposing (Position)
import Json.Decode exposing (Decoder, (:=))
import Random
import List exposing (map, any, head, tail, filter, foldl)
import Objects exposing (..)


-- CANVAS DIMENSIONS


width =
    1000


height =
    500


hudBackground =
    40


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
    if a < 20 then
        True
    else
        False


objectHit : ( Float, Float ) -> ( Float, Float ) -> Bool
objectHit ( px, py ) ( ox, oy ) =
    px <= ox + 20 && px >= ox - 20 && py <= oy + 20 && py >= oy - 20


listHit : ( Float, Float ) -> List Object -> Bool
listHit pos =
    foldl (||) False << map (\obj -> objectHit pos obj.pos)


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
