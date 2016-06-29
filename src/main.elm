module Main exposing (..)

import Html exposing (..)
import Html.App
import Element
import Collage exposing (..)
import Color exposing (..)
import Mouse exposing (Position)


-- MODEL

type alias Model =
    { pointer_position: Maybe Position }

init = 
    ( { pointer_position = Nothing }, Cmd.none )



-- UPDATE & SUBSCRIPTIONS

type Msg = Reset -- TODO: Add more Messages

update : Msg -> Model -> ( Model, Cmd Msg)
update msg model = ( model, Cmd.none )


subscriptions _ = Sub.none



-- VIEW

view : Model -> Html Msg
view model = 
    div [] [
        h1 [] [ Html.text "Super Spotlight" ],
        div [] [
            Element.toHtml <| collage 1000 800 [ filled black ( rect 1000 800 ) ]
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

