module Main exposing (..)

import Html exposing (..)
import Html.App
import Element
import Collage exposing (..)
import Color exposing (..)
import Mouse exposing (Position)


-- MODEL

type alias Model =
    { position: Position }

init = 
    ( { position = { x = 0, y = 0 } }, Cmd.none )



-- UPDATE & SUBSCRIPTIONS

type Msg = -- TODO: Add more Messages
          Reset
        | MouseMove Position 

update : Msg -> Model -> ( Model, Cmd Msg)
update msg ( { position } as model ) =
    case msg of
        Reset -> ( model, Cmd.none )
        MouseMove pos -> ( { model | position = pos }, Cmd.none )


subscriptions _ = Mouse.moves (\p -> MouseMove p)



-- VIEW

view : Model -> Html Msg
view ( { position } as  model ) = 
    div [] [
        h1 [] [ Html.text "Super Spotlight" ],
        div [] [
            Element.toHtml <| collage 1000 800 [ 
                filled black ( rect 1000 800 ),
                move (pos_to_float position) ( filled white (circle 50) )
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
