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
import Maybe exposing (map)


-- CUSTOM MODULES

import Objects exposing (..)
import Utility exposing (..)


-- MODEL
-- Canvas dimensions can be set in Utility.elm


type Model
    = PreGame
    | InGame GameModel
    | PostGame Int


type alias GameModel =
    { position : Position
    , clicked : Time
    , goodObjects : List Object
    , badObjects : List Object
    , vitalObject : Maybe Object
    , nextGoodSpawn : Time
    , nextBadSpawn : Time
    , nextVitalSpawn : Time
    , score : Int
    , lifes : Int
    , spawnNotification : Time
    }


type ObjType
    = Good
    | Bad
    | Vital


init : Model
init =
    PreGame


initGameModel : Position -> Model
initGameModel pos =
    InGame
        { position = pos
        , clicked = 0
        , goodObjects = []
        , badObjects = []
        , vitalObject = Nothing
        , nextGoodSpawn = 0
        , nextBadSpawn = 0
        , nextVitalSpawn = 0
        , score = 0
        , lifes = 5
        , spawnNotification = 0
        }



-- UPDATE & SUBSCRIPTIONS


type Msg
    = -- TODO: Add more Messages
      Reset
    | MouseMove Position
    | Click Position
    | Tick Time
    | NewGoodObject ( Float, Float )
    | NewBadObject ( Float, Float )
    | NewVitalObject ( Float, Float )
    | NextSpawnTime Int
    | NextSpawnTimeVit Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        PreGame ->
            case msg of
                Click pos ->
                    ( initGameModel pos, Cmd.batch [ newSpawnTime Good 2 4, newRandObject Vital ] )

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

                NewGoodObject pos ->
                    ( InGame { model | goodObjects = makeObject pos goodTtl good :: model.goodObjects }, Cmd.none )

                NewBadObject pos ->
                    ( InGame { model | badObjects = makeObject pos badTtl bad :: model.badObjects }, Cmd.none )

                NewVitalObject pos ->
                    ( InGame { model | vitalObject = Just <| makeObject pos goodTtl vital, spawnNotification = 300 * millisecond }, Cmd.none )

                NextSpawnTime tm ->
                    ( InGame { model | nextGoodSpawn = toFloat tm * second }, Cmd.none )

                NextSpawnTimeVit tm ->
                    ( InGame { model | nextVitalSpawn = toFloat tm * second }, Cmd.none )

        PostGame score ->
            case msg of
                Click pos ->
                    ( initGameModel pos, Cmd.batch [ newSpawnTime Good 2 4, newRandObject Vital ] )

                _ ->
                    ( PostGame score, Cmd.none )


tickUpdate : GameModel -> ( Model, Cmd Msg )
tickUpdate ({ clicked, goodObjects, badObjects, vitalObject, nextGoodSpawn, nextVitalSpawn, spawnNotification, lifes, score } as gm) =
    let
        updateTime : Time -> Time
        updateTime tm =
            if tm > 0 then
                tm - 100 * millisecond
            else
                tm

        updateObjects : List Object -> List Object
        updateObjects =
            filter (\obj -> obj.ttl > 0) << List.map (\obj -> { obj | ttl = obj.ttl - 100 * millisecond })

        updateVital : Maybe Object -> Maybe Object
        updateVital =
            (\obj ->
                case obj of
                    Nothing ->
                        Nothing

                    Just obj ->
                        if obj.ttl <= 0 then
                            Nothing
                        else
                            Just obj
            )
                << Maybe.map (\obj -> { obj | ttl = obj.ttl - 100 * millisecond })

        updateLifes : Maybe Object -> Int -> Int
        updateLifes obj lifes =
            case obj of
                Nothing ->
                    lifes

                Just o ->
                    if o.ttl - 100 <= 0 then
                        lifes - 1
                    else
                        lifes
    in
        if lifes <= 0 then
            ( PostGame score
            , Cmd.none
            )
        else if nextGoodSpawn <= 0 then
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , goodObjects = updateObjects goodObjects
                    , badObjects = updateObjects badObjects
                    , vitalObject = updateVital vitalObject
                    , nextVitalSpawn =
                        if nextVitalSpawn > 0 then
                            nextVitalSpawn - 100 * millisecond
                        else
                            nextVitalSpawn
                    , spawnNotification = updateTime spawnNotification
                    , lifes = updateLifes vitalObject lifes
                }
            , Cmd.batch [ newRandObject Good, newRandObject Bad, newSpawnTime Good 2 4 ]
            )
        else if nextVitalSpawn <= 0 && vitalObject == Nothing then
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , goodObjects = updateObjects goodObjects
                    , badObjects = updateObjects badObjects
                    , vitalObject = updateVital vitalObject
                    , nextGoodSpawn = nextGoodSpawn - 100 * millisecond
                    , spawnNotification = updateTime spawnNotification
                    , lifes = updateLifes vitalObject lifes
                }
            , Cmd.batch [ newRandObject Vital, newSpawnTime Vital 5 10 ]
            )
        else
            ( InGame
                { gm
                    | clicked = updateTime clicked
                    , goodObjects = updateObjects goodObjects
                    , badObjects = updateObjects badObjects
                    , vitalObject = updateVital vitalObject
                    , nextVitalSpawn =
                        if nextVitalSpawn > 0 then
                            nextVitalSpawn - 100 * millisecond
                        else
                            nextVitalSpawn
                    , nextGoodSpawn = nextGoodSpawn - 100 * millisecond
                    , spawnNotification = updateTime spawnNotification
                    , lifes = updateLifes vitalObject lifes
                }
            , Cmd.none
            )


clickUpdate : GameModel -> ( Model, Cmd Msg )
clickUpdate ({ position, clicked, goodObjects, badObjects, vitalObject, score, lifes } as gm) =
    let
        clickedGoodObj : Bool
        clickedGoodObj =
            listHit (correctOffset (posToFloat position)) goodObjects

        clickedBadObj : Bool
        clickedBadObj =
            listHit (correctOffset (posToFloat position)) badObjects

        clickedVitalObj : Bool
        clickedVitalObj =
            case vitalObject of
                Nothing ->
                    False

                Just obj ->
                    objectHit (correctOffset (posToFloat position)) obj.pos

        vanishObject : List Object -> List Object
        vanishObject =
            filter (\obj -> not <| objectHit (correctOffset (posToFloat position)) obj.pos)
    in
        if clickedGoodObj then
            ( InGame
                { gm
                    | score = score + 50
                    , goodObjects = vanishObject goodObjects
                }
            , Cmd.none
            )
        else if clickedBadObj then
            if lifes <= 1 then
                ( PostGame score
                , Cmd.none
                )
            else
                ( InGame
                    { gm
                        | clicked = (millisecond * 300)
                        , lifes = lifes - 1
                        , badObjects = vanishObject badObjects
                    }
                , Cmd.none
                )
        else if clickedVitalObj then
            ( InGame
                { gm
                    | score = score + 100
                    , vitalObject = Nothing
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
            gameSite preGameText

        InGame model ->
            inGameSite model

        PostGame score ->
            gameSite <| postGameText score


gameSite : Form -> Html Msg
gameSite f =
    div [ Html.Attributes.style [ "text-align" => "center" ] ]
        [ h1 [] [ Html.text "Super Spotlight" ]
        , div
            [ on "mousemove" (Json.Decode.map MouseMove offsetPosition)
            , on "click" (Json.Decode.map Click offsetPosition)
            , Html.Attributes.style [ "width" => px Utility.width, "height" => px Utility.height, "margin-left" => "auto", "margin-right" => "auto" ]
            ]
            [ Element.toHtml <|
                collage Utility.width
                    Utility.height
                    [ filled black (rect Utility.width Utility.height)
                    , f
                    ]
            ]
        ]


inGameSite : GameModel -> Html Msg
inGameSite ({ position, clicked, goodObjects, badObjects, vitalObject, spawnNotification } as model) =
    let
        getVital : Maybe Object -> List Object
        getVital obj =
            case obj of
                Just o ->
                    [ o ]

                Nothing ->
                    []
    in
        div [ Html.Attributes.style [ "text-align" => "center" ] ]
            [ h1 [] [ Html.text "Super Spotlight" ]
            , div
                [ on "mousemove" (Json.Decode.map MouseMove offsetPosition)
                , on "click" (Json.Decode.map Click offsetPosition)
                , Html.Attributes.style [ "width" => px Utility.width, "height" => px Utility.height, "cursor" => "none", "margin-left" => "auto", "margin-right" => "auto" ]
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
                            ++ List.map (\{ form } -> form) (goodObjects ++ badObjects ++ getVital vitalObject)
                            ++ [ move (correctOffset <| posToFloat position) <| group [ outlined (thickenLine <| solid black) (circle 20), outlined (solid black) (circle 1) ]
                               , alpha (spawnNotification / 300) <| moveY -20 <| outlined (thickenLine <| solid green) (rect (Utility.width - 2) (Utility.height - hudBackground))
                               ]
                        )
                ]
            ]


drawHUD : GameModel -> Form
drawHUD { score, lifes } =
    moveY (Utility.height / 2 - 10) <|
        Collage.text <|
            Text.height 18 <|
                monospace <|
                    color white <|
                        fromString <|
                            "SCORE: "
                                ++ toString score
                                ++ "    LIFES: "
                                ++ toString lifes


preGameText : Form
preGameText =
    Collage.group
        [ Collage.text <| Text.height 40 (color white <| fromString "Super Spotlight")
        , moveY -50 (Collage.text <| Text.height 16 <| monospace (color white <| fromString "click to start"))
        ]


postGameText : Int -> Form
postGameText score =
    moveY 20 <|
        Collage.group
            [ Collage.text <|
                Text.height 40 (color white <| fromString "GAME OVER")
            , moveY -40 (Collage.text <| Text.height 16 (color white <| fromString <| "final score: " ++ toString score))
            , moveY -60 (Collage.text <| monospace (color white <| fromString "click to play again"))
            ]



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


newRandObject : ObjType -> Cmd Msg
newRandObject ty =
    case ty of
        Good ->
            Random.generate NewGoodObject randPos

        Bad ->
            Random.generate NewBadObject randPos

        Vital ->
            Random.generate NewVitalObject randPos


newSpawnTime : ObjType -> Int -> Int -> Cmd Msg
newSpawnTime ty a b =
    case ty of
        Vital ->
            Random.generate NextSpawnTimeVit <| Random.int a b

        _ ->
            Random.generate NextSpawnTime <| Random.int a b
