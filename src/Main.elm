module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, (:=))
import Element
import Collage exposing (..)
import Color exposing (..)
import Mouse exposing (Position)
import Time exposing (Time, second, millisecond)


width =
    1000


height =
    800



-- MODEL


type alias Model =
    { position : Position, clicked : Time }


init =
    ( { position = Position (truncate <| width / 2) (truncate <| height / 2), clicked = 0 }, Cmd.none )



-- UPDATE & SUBSCRIPTIONS


type Msg
    = -- TODO: Add more Messages
      Reset
    | MouseMove Position
    | Click
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ clicked } as model) =
    case msg of
        Reset ->
            init

        MouseMove pos ->
            ( { model | position = pos }, Cmd.none )

        Click ->
            ( { model | clicked = (millisecond * 300) }, Cmd.none )

        Tick _ ->
            ( { model
                | clicked =
                    (if clicked > 0 then
                        clicked - 100
                     else
                        clicked
                    )
              }
            , Cmd.none
            )


subscriptions _ =
    Time.every (millisecond * 100) Tick



-- VIEW

(=>) = (,)

px : Int -> String
px s = toString s ++ "px"


view : Model -> Html Msg
view ({ position, clicked } as model) =
    div []
        [ h1 [] [ Html.text "Super Spotlight" ]
        , div [ on "mousemove" (Json.Decode.map MouseMove offsetPosition), onClick Click, style [ "width" => px width, "height" => px height, "cursor" => "none" ] ]
            [ Element.toHtml
                <| collage width
                    height
                    [ filled black (rect width height)
                    , move (correct_offset <| pos_to_float position)
                        (filled
                            (if clicked > 0 then
                                red
                             else
                                white
                            )
                            (circle 50)
                        )
                    ]
            ]
        ]


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UTILITY


pos_to_float : Position -> ( Float, Float )
pos_to_float p =
    ( toFloat <| p.x, toFloat <| -p.y )


correct_offset : ( Float, Float ) -> ( Float, Float )
correct_offset =
    (\( x, y ) -> ( x - width / 2, y + height / 2 ))


offsetPosition : Decoder Position
offsetPosition =
    Json.Decode.object2 Position
        ("offsetX" := Json.Decode.int)
        ("offsetY" := Json.Decode.int)
