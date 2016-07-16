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
import Random
import List exposing (map, any, head, tail, filter)


-- CUSTOM MODULES

import Objects exposing (..)
import Utility exposing (..)


-- MODEL
-- Canvas dimensions can be set in Utility.elm


type Model
    = PreGame
    | InGame GameModel
    | PostGame


type alias GameModel =
    { position : Position, clicked : Time, objects : List Object, nextSpawn : Time, score : Int, lives : Int, spawnNotification : Time }


init : Model
init =
    PreGame


initGameModel : Position -> Model
initGameModel pos =
    InGame { position = pos, clicked = 0, objects = [], nextSpawn = 5000 * millisecond, score = 0, lives = 5, spawnNotification = 0 }



-- UPDATE & SUBSCRIPTIONS


type Msg
    = -- TODO: Add more Messages
      Reset
    | MouseMove Position
    | Click Position
    | Tick Time
    | NewObject ( Float, Float )
    | NextSpawnTime Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreGame ->
            case msg of
                Click pos ->
                    ( initGameModel pos, newRandObject )

                _ ->
                    ( PreGame, Cmd.none )

        InGame ({ clicked, position, objects } as model) ->
            case msg of
                Reset ->
                    ( init, Cmd.none )

                MouseMove pos ->
                    ( InGame { model | position = pos }, Cmd.none )

                Click _ ->
                    ( InGame
                        { model
                            | clicked =
                                if any rightDist (listDist (correctOffset (posToFloat position)) objects) then
                                    (millisecond * 300)
                                else
                                    clicked
                        }
                    , Cmd.none
                    )

                Tick _ ->
                    tickUpdate model

                NewObject pos ->
                    ( InGame { model | objects = makeObject pos good :: model.objects, spawnNotification = 300 * millisecond }, Cmd.none )

                NextSpawnTime tm ->
                    ( InGame { model | nextSpawn = toFloat tm * second }, Cmd.none )

        PostGame ->
            case msg of
                Click pos ->
                    ( initGameModel pos, Cmd.none )

                _ ->
                    ( PostGame, Cmd.none )


tickUpdate : GameModel -> ( Model, Cmd Msg )
tickUpdate ({ clicked, objects, nextSpawn, spawnNotification } as gm) =
    let
        updateTime : Time -> Time
        updateTime tm =
            if tm > 0 then
                tm - 100 * millisecond
            else
                tm

        updateObjects : List Object -> List Object
        updateObjects =
            filter (\obj -> obj.ttl > 0) << map (\obj -> { obj | ttl = obj.ttl - 100 * millisecond })
    in
        if nextSpawn == 0 then
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , objects = updateObjects objects
                    , spawnNotification = updateTime spawnNotification
                }
            , Cmd.batch [ newRandObject, newSpawnTime ]
            )
        else
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , objects = updateObjects objects
                    , nextSpawn = nextSpawn - 100 * millisecond
                    , spawnNotification = updateTime spawnNotification
                }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        InGame _ ->
            Time.every (millisecond * 100) Tick

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        PreGame ->
            div []
                [ h1 [] [ Html.text "Super Spotlight" ]
                , div
                    [ on "mousemove" (Json.Decode.map MouseMove offsetPosition)
                    , on "click" (Json.Decode.map Click offsetPosition)
                    , Html.Attributes.style [ "width" => px Utility.width, "height" => px Utility.height ]
                    ]
                    [ Element.toHtml <|
                        collage Utility.width
                            Utility.height
                            [ filled black (rect Utility.width Utility.height)
                            , Collage.text <| Text.height 40 (color white <| fromString "Super Spotlight")
                            , moveY -50 (Collage.text <| monospace (color white <| fromString "click to start"))
                            ]
                    ]
                ]

        InGame ({ position, clicked, objects, spawnNotification } as model) ->
            div []
                [ h1 [] [ Html.text "Super Spotlight" ]
                , div
                    [ on "mousemove" (Json.Decode.map MouseMove offsetPosition)
                    , on "click" (Json.Decode.map Click offsetPosition)
                    , Html.Attributes.style [ "width" => px Utility.width, "height" => px Utility.height, "cursor" => "none" ]
                    ]
                    [ Element.toHtml <|
                        collage Utility.width
                            Utility.height
                            ([ filled black (rect Utility.width Utility.height)
                             , move (correctOffset <| posToFloat position)
                                (filled
                                    (if clicked > 0 then
                                        red
                                     else
                                        white
                                    )
                                    (circle 80)
                                )
                             , moveY (Utility.height / 2 - 20) <| filled black (rect Utility.width hudBackground)
                               -- Hintergrund für HUD (wird sonst vom Lichtkegel übermalt)
                             , drawHUD model
                             ]
                                ++ map (\{ form } -> form) objects
                                ++ [ move (correctOffset <| posToFloat position) <| group [ outlined (thickenLine <| solid black) (circle 20), outlined (solid black) (circle 1) ]
                                   , alpha (spawnNotification / 300) <| moveY -20 <| outlined (thickenLine <| solid green) (rect (Utility.width - 2) (Utility.height - hudBackground))
                                   ]
                            )
                    ]
                ]

        PostGame ->
            div []
                [ h1 [] [ Html.text "Super Spotlight" ]
                , div
                    [ on "mousemove" (Json.Decode.map MouseMove offsetPosition)
                    , on "click" (Json.Decode.map Click offsetPosition)
                    , Html.Attributes.style [ "Utility.width" => px Utility.width, "Utility.height" => px Utility.height ]
                    ]
                    [ Element.toHtml <|
                        collage Utility.width
                            Utility.height
                            [ filled black (rect Utility.width Utility.height)
                            , Collage.text <|
                                Text.height 40 (color white <| fromString "Game Over")
                            , moveY -50 (Collage.text <| monospace (color white <| fromString "click to play again"))
                            ]
                    ]
                ]


drawHUD : GameModel -> Form
drawHUD { score, lives } =
    moveY (Utility.height / 2 - 10) <| Collage.text <| Text.height 18 <| monospace <| color white <| fromString <| "SCORE: " ++ toString score ++ "    LIVES: " ++ toString lives



-- MAIN


main : Program Never
main =
    Html.App.program
        { init = ( init, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UTILITY


newRandObject : Cmd Msg
newRandObject =
    Random.generate NewObject randPos


newSpawnTime : Cmd Msg
newSpawnTime =
    Random.generate NextSpawnTime <| Random.int 2 4
