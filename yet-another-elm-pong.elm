module Main exposing (..)

import Html exposing (Html, text)
import Keyboard.Extra as KB
import AnimationFrame
import Time exposing (Time, inSeconds)
import Char exposing (KeyCode)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { is_running : Bool
    , ball : Ball
    , playerA : Player
    , playerB : Player
    , input : Input
    , steps : Int
    }


type alias Input =
    { space : Bool
    , paddleAUp : Int
    , paddleADown : Int
    , paddleBUp : Int
    , paddleBDown : Int
    }


type Msg
    = TimeDiff Time
    | KeyUp KB.Key
    | KeyDown KB.Key


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if model.steps > 100 then
    --     Sub.none
    -- else
    Sub.batch
        [ AnimationFrame.diffs TimeDiff
        , KB.downs KeyDown
        , KB.ups KeyUp
        ]


model : Model
model =
    { is_running = False
    , ball = { x = 0, y = 0, vx = 200, vy = 200 }
    , playerA = player (20 - halfWidth)
    , playerB = player (halfWidth - 20)
    , steps = 0
    , input =
        { space = False
        , paddleAUp = 0
        , paddleADown = 0
        , paddleBUp = 0
        , paddleBDown = 0
        }
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model0 =
    let
        model =
            { model0 | steps = model0.steps + 1 }
    in
        case msg of
            TimeDiff dt ->
                ( stepGame dt model, Cmd.none )

            KeyDown keyCode ->
                ( keyDown keyCode model, Cmd.none )

            KeyUp keyCode ->
                ( keyUp keyCode model, Cmd.none )


keyDown : KB.Key -> Model -> Model
keyDown key model =
    let
        input =
            model.input
    in
        case key of
            KB.Space ->
                { model | input = { input | space = True } }

            KB.ArrowUp ->
                { model | input = { input | paddleBUp = 1 } }

            KB.ArrowDown ->
                { model | input = { input | paddleBDown = 1 } }

            KB.CharW ->
                { model | input = { input | paddleAUp = 1 } }

            KB.CharS ->
                { model | input = { input | paddleADown = 1 } }

            _ ->
                model


keyUp : KB.Key -> Model -> Model
keyUp key model =
    let
        input =
            model.input
    in
        case key of
            KB.Space ->
                { model | input = { input | space = False } }

            KB.ArrowUp ->
                { model | input = { input | paddleBUp = 0 } }

            KB.ArrowDown ->
                { model | input = { input | paddleBDown = 0 } }

            KB.CharW ->
                { model | input = { input | paddleAUp = 0 } }

            KB.CharS ->
                { model | input = { input | paddleADown = 0 } }

            _ ->
                model


( gameWidth, gameHeight ) =
    ( 600, 400 )
( halfWidth, halfHeight ) =
    ( 300, 200 )


type alias Object a =
    { a
        | x : Float
        , y : Float
        , vx : Float
        , vy : Float
    }


type alias Ball =
    Object {}


type alias Player =
    Object { score : Int }


player : Float -> Player
player x =
    { x = x, y = 0, vx = 0, vy = 0, score = 0 }



-- are n and m near each other?
-- specifically are they within c of each other?


near : Float -> Float -> Float -> Bool
near n c m =
    m >= n - c && m <= n + c



-- is the ball within a paddle?


within : Ball -> Player -> Bool
within ball player =
    near player.x 13 ball.x
        && near player.y 25 ball.y



-- change the direction of a velocity based on collisions


stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v
    else if upperCollision then
        -(abs v)
    else
        v



-- step the position of an object based on its velocity and a timestep


stepObj : Time -> Object a -> Object a
stepObj t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * t
        , y = y + vy * t
    }



-- move a ball forward, detecting collisions with either paddle


stepBall : Time -> Ball -> Player -> Player -> Ball
stepBall t ({ x, y, vx, vy } as ball) player1 player2 =
    if not (ball.x |> near 0 halfWidth) then
        { ball | x = 0, y = 0 }
    else
        stepObj t
            { ball
                | vx =
                    stepV vx (within ball player1) (within ball player2)
                , vy =
                    stepV vy (y < 7 - halfHeight) (y > halfHeight - 7)
            }



-- step a player forward, making sure it does not fly off the court


stepPlayer : Time -> Int -> Int -> Player -> Player
stepPlayer t dir points player0 =
    let
        player1 =
            stepObj t { player0 | vy = toFloat dir * 200 }

        y1 =
            clamp (22 - halfHeight) (halfHeight - 22) player1.y

        score1 =
            player0.score + points
    in
        { player1 | y = y1, score = score1 }


stepGame : Time -> Model -> Model
stepGame dt model =
    let
        { space, paddleAUp, paddleADown, paddleBUp, paddleBDown } =
            model.input

        delta =
            inSeconds dt

        paddleA =
            paddleAUp - paddleADown

        paddleB =
            paddleBUp - paddleBDown

        scoreA =
            if model.ball.x > halfWidth then
                1
            else
                0

        scoreB =
            if model.ball.x < -halfWidth then
                1
            else
                0
    in
        { model
            | is_running =
                if model.input.space then
                    True
                else if (scoreA > 0 || scoreB > 0) then
                    False
                else
                    model.is_running
            , ball =
                if model.is_running then
                    stepBall delta model.ball model.playerA model.playerB
                else
                    model.ball
            , playerA = stepPlayer delta paddleA scoreA model.playerA
            , playerB = stepPlayer delta paddleB scoreB model.playerB
        }



-- helper values


msg =
    "SPACE to start, WS and &uarr;&darr; to move"



-- shared function for rendering objects


pongGreen =
    "#3C647B"


textGreen =
    "#A0C8A0"


place : Object a -> List (Svg Msg) -> Svg Msg
place obj list =
    g [ transform ("translate(" ++ toString (obj.x + halfWidth) ++ " " ++ toString (-obj.y + halfHeight) ++ ")") ] list


paddleSvg : Svg Msg
paddleSvg =
    rect [ x "-8", y "-20", width "16", height "40", fill "white" ] []


ballSvg : Svg Msg
ballSvg =
    circle [ r "10", fill "white" ] []


helpSvg : List (Svg Msg)
helpSvg =
    [ text_ [ x "20", y "50", fill textGreen, fontSize "36", textAnchor "start" ] [ Svg.text "W" ]
    , text_ [ x (toString <| gameWidth - 20), y "50", fill textGreen, fontSize "36", textAnchor "end" ] [ Svg.text "Up" ]
    , text_ [ x "20", y (toString <| gameHeight - 20), fill textGreen, fontSize "36", textAnchor "start" ] [ Svg.text "S" ]
    , text_ [ x (toString <| gameWidth - 20), y (toString <| gameHeight - 20), fill textGreen, fontSize "36", textAnchor "end" ] [ Svg.text "Down" ]
    , text_ [ x (toString <| gameWidth / 2), y "50", fill textGreen, fontSize "36", textAnchor "middle" ] [ Svg.text "Press SPACE" ]
    ]


view : Model -> Html Msg
view model =
    svg [ viewBox ("0 0 " ++ toString gameWidth ++ " " ++ toString gameHeight) ]
        [ rect [ x "0", y "0", width "100%", height "100%", fill pongGreen ] []
        , g []
            (if model.is_running then
                []
             else
                helpSvg
            )
        , text_ [ x (toString <| gameWidth / 4), y (toString <| gameHeight / 2 + 20), fill textGreen, fontSize "50", textAnchor "middle" ] [ Svg.text <| toString model.playerA.score ]
        , text_ [ x (toString <| gameWidth / 4 * 3), y (toString (gameHeight / 2 + 20)), fill textGreen, fontSize "50", textAnchor "middle" ] [ Svg.text <| toString model.playerB.score ]
        , place model.ball [ ballSvg ]
        , place model.playerA [ paddleSvg ]
        , place model.playerB [ paddleSvg ]
        ]
