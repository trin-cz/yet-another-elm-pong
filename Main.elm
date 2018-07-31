module Main exposing (..)

import Html exposing (Html, text)
import Keyboard.Extra as KB
import AnimationFrame
import Time exposing (Time, inSeconds)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Collision exposing (..)


main : Program Never Model Msg
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


dims :
    { ballW : Float
    , paddleW : Float
    , paddleH : Float
    , gameW : Float
    , gameH : Float
    , wall : Float
    }
dims =
    { ballW = 10
    , paddleW = 10
    , paddleH = 40
    , gameW = 600
    , gameH = 400
    , wall = 100
    }


paddleSpeed : Float
paddleSpeed =
    200


cols : { pongGreen : String, textGreen : String }
cols =
    { pongGreen = "#3C647B"
    , textGreen = "#A0C8A0"
    }


walls : { left : Rectangle, right : Rectangle, top : Rectangle, bottom : Rectangle }
walls =
    { top =
        Rectangle
            { cx = 0
            , cy = 0 - dims.gameH / 2 - dims.wall / 2
            , w = dims.gameW + 2 * dims.wall
            , h = dims.wall
            }
    , bottom =
        Rectangle
            { cx = 0
            , cy = dims.gameH / 2 + dims.wall / 2
            , w = dims.gameW + 2 * dims.wall
            , h = dims.wall
            }
    , left =
        Rectangle
            { cx = 0 - dims.gameW / 2 - dims.wall
            , cy = 0
            , w = dims.wall
            , h = dims.gameH + 2 * dims.wall
            }
    , right =
        Rectangle
            { cx = dims.gameW / 2 + dims.wall
            , cy = 0
            , w = dims.wall
            , h = dims.gameH + 2 * dims.wall
            }
    }


fromBall : Object a -> Rectangle
fromBall ball =
    Rectangle { cx = ball.x, cy = ball.y, w = dims.ballW, h = dims.ballW }


fromPaddle : Object a -> Rectangle
fromPaddle paddle =
    Rectangle { cx = paddle.x, cy = paddle.y, w = dims.paddleW, h = dims.paddleH }


model : Model
model =
    { is_running = False
    , ball = { x = 0, y = 0, vx = 200, vy = 200 }
    , playerA = player (20 - dims.gameW / 2)
    , playerB = player (dims.gameW / 2 - 20)
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



-- step the position of an object based on its velocity and a timestep


stepObj : Time -> Object a -> Object a
stepObj t ({ x, y, vx, vy } as obj) =
    { obj
        | x = x + vx * t
        , y = y + vy * t
    }



-- move a ball forward, detecting collisions with either paddle


handleBallCollisions : Model -> Ball
handleBallCollisions model =
    let
        ball =
            model.ball

        ball_rect =
            fromBall ball
    in
        if rectangleIsCollision ball_rect walls.top then
            { ball | vy = abs ball.vy }
        else if rectangleIsCollision ball_rect walls.bottom then
            { ball | vy = 0 - abs ball.vy }
        else if rectangleIsCollision ball_rect (fromPaddle model.playerA) then
            { ball | vx = abs ball.vx }
        else if rectangleIsCollision ball_rect (fromPaddle model.playerB) then
            { ball | vx = 0 - abs ball.vx }
        else
            ball



-- step a player forward, making sure it does not fly off the court


handlePaddleCollision : Player -> Player
handlePaddleCollision player =
    let
        paddle_rect =
            fromPaddle player
    in
        { player
            | vy =
                if rectangleIsCollision paddle_rect walls.top then
                    Basics.max 0 player.vy
                else if rectangleIsCollision paddle_rect walls.bottom then
                    Basics.min 0 player.vy
                else
                    player.vy
        }


handleCollisions : Model -> Model
handleCollisions model =
    { model
        | playerA = handlePaddleCollision model.playerA
        , playerB = handlePaddleCollision model.playerB
        , ball = handleBallCollisions model
    }


stepGame : Time -> Model -> Model
stepGame dt model =
    handleScores <| updatePositions dt <| handleCollisions <| handleInput model


handleInput : Model -> Model
handleInput model =
    let
        { space, paddleAUp, paddleADown, paddleBUp, paddleBDown } =
            model.input

        paddleAdir =
            paddleAUp - paddleADown

        paddleBdir =
            paddleBUp - paddleBDown

        playerA =
            model.playerA

        playerB =
            model.playerB
    in
        { model
            | is_running =
                if space then
                    True
                else
                    model.is_running
            , playerA = { playerA | vy = toFloat paddleAdir * paddleSpeed }
            , playerB = { playerB | vy = toFloat paddleBdir * paddleSpeed }
        }


updatePositions : Time -> Model -> Model
updatePositions dt model =
    let
        delta =
            inSeconds dt
    in
        if model.is_running then
            { model
                | ball = stepObj delta model.ball
                , playerA = stepObj delta model.playerA
                , playerB = stepObj delta model.playerB
            }
        else
            model


handleScores : Model -> Model
handleScores model =
    let
        ball_rect =
            fromBall model.ball

        scoreA =
            if rectangleIsCollision ball_rect walls.left then
                1
            else
                0

        scoreB =
            if rectangleIsCollision ball_rect walls.right then
                1
            else
                0

        ball =
            model.ball

        playerA =
            model.playerA

        playerB =
            model.playerB
    in
        if (scoreA > 0 || scoreB > 0) then
            { model
                | is_running = False
                , ball = { ball | x = 0, y = 0 }
                , playerA = { playerA | score = playerA.score + scoreA }
                , playerB = { playerB | score = playerB.score + scoreB }
            }
        else
            model



-- shared function for rendering objects


place : Object a -> List (Svg Msg) -> Svg Msg
place obj list =
    g [ transform ("translate(" ++ toString (obj.x + dims.gameW / 2) ++ " " ++ toString (-obj.y + dims.gameH / 2) ++ ")") ] list


paddleSvg : Svg Msg
paddleSvg =
    rect
        [ x <| toString <| 0 - dims.paddleW / 2
        , y <| toString <| 0 - dims.paddleH / 2
        , width <| toString dims.paddleW
        , height <| toString dims.paddleH
        , fill "white"
        ]
        []


ballSvg : Svg Msg
ballSvg =
    circle [ r <| toString dims.ballW, fill "white" ] []


helpSvg : List (Svg Msg)
helpSvg =
    [ text_ [ x "20", y "50", fill cols.textGreen, fontSize "36", textAnchor "start" ] [ Svg.text "W" ]
    , text_ [ x (toString <| dims.gameW - 20), y "50", fill cols.textGreen, fontSize "36", textAnchor "end" ] [ Svg.text "Up" ]
    , text_ [ x "20", y (toString <| dims.gameH - 20), fill cols.textGreen, fontSize "36", textAnchor "start" ] [ Svg.text "S" ]
    , text_ [ x (toString <| dims.gameW - 20), y (toString <| dims.gameH - 20), fill cols.textGreen, fontSize "36", textAnchor "end" ] [ Svg.text "Down" ]
    , text_ [ x (toString <| dims.gameW / 2), y "50", fill cols.textGreen, fontSize "36", textAnchor "middle" ] [ Svg.text "Press SPACE" ]
    ]


view : Model -> Html Msg
view model =
    svg [ viewBox ("0 0 " ++ toString dims.gameW ++ " " ++ toString dims.gameH) ]
        [ rect [ x "0", y "0", width "100%", height "100%", fill cols.pongGreen ] []
        , g []
            (if model.is_running then
                []
             else
                helpSvg
            )
        , text_ [ x (toString <| dims.gameW / 4), y <| toString <| dims.gameH / 2 + 20, fill cols.textGreen, fontSize "50", textAnchor "middle" ] [ Svg.text <| toString model.playerA.score ]
        , text_ [ x (toString <| dims.gameW / 4 * 3), y <| toString <| dims.gameH / 2 + 20, fill cols.textGreen, fontSize "50", textAnchor "middle" ] [ Svg.text <| toString model.playerB.score ]
        , place model.ball [ ballSvg ]
        , place model.playerA [ paddleSvg ]
        , place model.playerB [ paddleSvg ]
        ]
