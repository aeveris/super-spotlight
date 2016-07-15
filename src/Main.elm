module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, (:=))
import Element
import Collage exposing (..)
import Text exposing (..)
import Color exposing (..)
import Mouse exposing (Position)
import Time exposing (Time, second, millisecond)
import List exposing (map)


-- OWN MODULES

import Objects exposing (..)


width =
    1000


height =
    800



-- MODEL


type Model
    = PreGame
    | InGame GameModel
    | PostGame


type alias GameModel =
    { position : Position, clicked : Time, objects : List Object, nextSpawn : Time }


init : Model
init =
    PreGame


initGameModel : Model
initGameModel =
    InGame { position = Position (truncate <| width / 2) (truncate <| height / 2), clicked = 0, objects = [], nextSpawn = 0 }



-- UPDATE & SUBSCRIPTIONS


type Msg
    = -- TODO: Add more Messages
      Reset
    | MouseMove Position
    | Click
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreGame ->
            case msg of
                Click ->
                    ( initGameModel, Cmd.none )

                _ ->
                    ( PreGame, Cmd.none )

        InGame ({ clicked } as model) ->
            case msg of
                Reset ->
                    ( init, Cmd.none )

                MouseMove pos ->
                    ( InGame { model | position = pos }, Cmd.none )

                Click ->
                    ( InGame { model | clicked = (millisecond * 300) }, Cmd.none )

                Tick _ ->
                    ( InGame
                        { model
                            | clicked =
                                (if clicked > 0 then
                                    clicked - 100
                                 else
                                    clicked
                                )
                        }
                    , Cmd.none
                    )

        PostGame ->
            case msg of
                Click ->
                    ( initGameModel, Cmd.none )

                _ ->
                    ( PostGame, Cmd.none )


subscriptions _ =
    Time.every (millisecond * 100) Tick



-- VIEW


(=>) =
    (,)


px : Int -> String
px s =
    toString s ++ "px"


view : Model -> Html Msg
view model =
    case model of
        PreGame ->
            div []
                [ h1 [] [ Html.text "Super Spotlight" ]
                , div [ on "mousemove" (Json.Decode.map MouseMove offsetPosition), onClick Click, Html.Attributes.style [ "width" => px width, "height" => px height, "cursor" => "none" ] ]
                    [ Element.toHtml <|
                        collage width
                            height
                            [ filled black (rect width height), Collage.text <| Text.height 40 (color white <| fromString "Super Spotlight"), moveY -50 (Collage.text <| monospace (color white <| fromString "click to start")) ]
                    ]
                ]

        InGame ({ position, clicked, objects } as model) ->
            div []
                [ h1 [] [ Html.text "Super Spotlight" ]
                , div [ on "mousemove" (Json.Decode.map MouseMove offsetPosition), onClick Click, Html.Attributes.style [ "width" => px width, "height" => px height, "cursor" => "none" ] ]
                    [ Element.toHtml <|
                        collage width
                            height
                            ([ filled black (rect width height)
                             , move (correctOffset <| posToFloat position)
                                (filled
                                    (if clicked > 0 then
                                        red
                                     else
                                        white
                                    )
                                    (circle 50)
                                )
                             ]
                                ++ map (\{ form } -> form) objects
                            )
                    ]
                ]

        PostGame ->
            div []
                [ h1 [] [ Html.text "Super Spotlight" ]
                , div [ on "mousemove" (Json.Decode.map MouseMove offsetPosition), onClick Click, Html.Attributes.style [ "width" => px width, "height" => px height, "cursor" => "none" ] ]
                    [ Element.toHtml <|
                        collage width
                            height
                            [ filled black (rect width height), Collage.text <| Text.height 40 (color white <| fromString "Game Over"), moveY -50 (Collage.text <| monospace (color white <| fromString "click to play again")) ]
                    ]
                ]


main : Program Never
main =
    Html.App.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UTILITY


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
