module Main exposing (..)

import Html exposing (..)
import Html.App
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, (:=))
import Element
import Collage exposing (..)
import Color exposing (..)
import Mouse exposing (Position)

width = 1000
height = 800



-- MODEL

type alias Model =
    { position: Position }

init = 
    ( { position = Position (truncate <| width/2) (truncate <| height/2)  }, Cmd.none )



-- UPDATE & SUBSCRIPTIONS

type Msg = -- TODO: Add more Messages
          Reset
        | MouseMove Position 

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( { position } as model ) =
    case msg of
        Reset -> ( model, Cmd.none )
        MouseMove pos -> ( { model | position = pos }, Cmd.none )


subscriptions _ = Sub.none



-- VIEW

view : Model -> Html Msg
view ( { position } as  model ) = 
    div [] [
        h1 [] [ Html.text "Super Spotlight" ],
        div [ on "mousemove" (Json.Decode.map MouseMove offsetPosition)] [
            Element.toHtml <| collage width height [
                filled black ( rect width height ),
                move (correct_offset <| pos_to_float position) ( filled white (circle 50) )
                ]
            ]
        ]






main : Program Never
main =
      Html.App.program {
            init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
          }

-- UTILITY

pos_to_float : Position -> (Float, Float)
pos_to_float p = (toFloat <| p.x, toFloat <| -p.y)

correct_offset : (Float, Float) -> (Float, Float)
correct_offset = (\( x, y ) -> (x - width/2, y + height/2))

offsetPosition : Decoder Position
offsetPosition =
        Json.Decode.object2 Position
        ("offsetX" := Json.Decode.int)
        ("offsetY" := Json.Decode.int)
