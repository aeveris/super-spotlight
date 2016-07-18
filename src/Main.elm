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
    { position : Position, clicked : Time, goodObjects : List Object, badObjects : List Object,
    nextGoodSpawn : Time, nextBadSpawn : Time, score : Int, lives : Int, spawnNotification : Time }


init : Model
init =
    PreGame


initGameModel : Position -> Model
initGameModel pos =
    InGame { position = pos, clicked = 0, goodObjects = [], badObjects = [], nextGoodSpawn = 0, nextBadSpawn = 0, score = 0, lives = 5, spawnNotification = 0 }



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
                    ( initGameModel pos, newSpawnTime 2 4 )

                _ ->
                    ( PreGame, Cmd.none )

        InGame ({ clicked, position, goodObjects } as model) ->
            case msg of
                Reset ->
                    ( init, Cmd.none )

                MouseMove pos ->
                    ( InGame { model | position = pos }, Cmd.none )

                Click _ ->
                    clickUpdate model

                Tick _ ->
                    tickUpdate model

                NewObject pos ->
                    ( InGame { model | goodObjects = makeObject pos good :: model.goodObjects, spawnNotification = 300 * millisecond }, Cmd.none )

                NextSpawnTime tm ->
                    ( InGame { model | nextGoodSpawn = toFloat tm * second }, Cmd.none )

        PostGame ->
            case msg of
                Click pos ->
                    ( initGameModel pos, newSpawnTime 2 4 )

                _ ->
                    ( PostGame, Cmd.none )


tickUpdate : GameModel -> ( Model, Cmd Msg )
tickUpdate ({ clicked, goodObjects, nextGoodSpawn, spawnNotification } as gm) =
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
        if nextGoodSpawn == 0 then
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , goodObjects = updateObjects goodObjects
                    , spawnNotification = updateTime spawnNotification
                }
            , Cmd.batch [ newRandObject, newSpawnTime 2 4 ]
            )
        else
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , goodObjects = updateObjects goodObjects
                    , nextGoodSpawn = nextGoodSpawn - 100 * millisecond
                    , spawnNotification = updateTime spawnNotification
                }
            , Cmd.none
            )

clickUpdate : GameModel -> (Model, Cmd Msg)
clickUpdate ( { position, clicked, goodObjects, score } as gm ) =
  let
    clickedGoodObj =
      if any rightDist (listDist (correctOffset (posToFloat position)) goodObjects) then
        True
      else
        False

    vanishObject : List Object -> List Object
    vanishObject =
      filter (\obj -> not (rightDist( objDist (correctOffset (posToFloat position)) obj)))


 in
  if clickedGoodObj then
  ( InGame
      { gm
          | clicked = (millisecond * 300)
          , score = score + 1
          , goodObjects = vanishObject goodObjects

      }
  , Cmd.none
  )
  else
    ( InGame
        { gm
            | clicked = clicked

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
            preGameSite

        InGame model ->
            inGameSite model

        PostGame ->
            postGameSite


preGameSite : Html Msg
preGameSite =
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
                    , Collage.text <|
                        Text.height 40 (color white <| fromString "Super Spotlight")
                    , moveY -50 (Collage.text <| monospace (color white <| fromString "click to start"))
                    ]
            ]
        ]


postGameSite : Html Msg
postGameSite =
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


inGameSite : GameModel -> Html Msg
inGameSite ({ position, clicked, goodObjects, spawnNotification } as model) =
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
                        ++ map (\{ form } -> form) goodObjects
                        ++ [ move (correctOffset <| posToFloat position) <| group [ outlined (thickenLine <| solid black) (circle 20), outlined (solid black) (circle 1) ]
                           , alpha (spawnNotification / 300) <| moveY -20 <| outlined (thickenLine <| solid green) (rect (Utility.width - 2) (Utility.height - hudBackground))
                           ]
                    )
            ]
        ]


drawHUD : GameModel -> Form
drawHUD { score, lives } =
    moveY (Utility.height / 2 - 10) <|
        Collage.text <|
            Text.height 18 <|
                monospace <|
                    color white <|
                        fromString <|
                            "SCORE: "
                                ++ toString score
                                ++ "    LIVES: "
                                ++ toString lives



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


newSpawnTime : Int -> Int -> Cmd Msg
newSpawnTime a b =
    Random.generate NextSpawnTime <| Random.int a b
