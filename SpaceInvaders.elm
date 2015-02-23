module SpaceInvaders where

import List ((::))
import List
import Color (..)
import Signal ((<~),(~),Signal)
import Signal
import Window
import Mouse
import Keyboard
import Text (plainText)
import Text as T
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input (button, customButton)
import Time (Time)
import Time


areaW = 500
areaH = 500

type Move = MoveL | MoveR | Idle

type LaserStatus = Ready | Shooting | Collision

type State = Play | Pause

type alias Player =
  { x    : Float
  , y    : Float
  , vx   : Float
  , vy   : Float
  , move : Move
  }

type alias Laser =
  { x_laser : Float
  , y_laser : Float
  , vx      : Float
  , vy      : Float
  , status  : LaserStatus
  , fired   : Time
  }

type alias Game =
  { state  : State
  , player : Player
  , laser  : Laser
  , score  : Float
  }

initGame : Game
initGame =
  { state  = Pause
  , player = 
    { x    = 0
    , y    = -220
    , vx   = 0
    , vy   = 0
    , move = Idle
    }
  , laser  = 
    { x_laser = 0
    , y_laser = -215
    , vx      = 0
    , vy      = 0
    , status  = Ready
    , fired   = 0
    }
  , score  = 0
  }

type alias Input =
  { start     : Bool
  , pause     : Bool
  , playerDir : Int
  , shoot     : Bool
  , delta     : Time
  }

-- UPDATE

update_game : Input -> Game -> Game
update_game input game =
  let
    {start, pause, playerDir, shoot, delta} = input
    {state, player, laser, score} = game
    newState = state
      |> start_game start
      |> pause_game pause
    newPlayer = 
      case state of
        Pause -> player
        Play -> 
          player
            |> update_vel_player   playerDir
            |> update_move_player  playerDir
            |> update_pos_player   delta
    newLaser = 
      case state of
        Pause -> laser
        Play -> 
          laser
            |> update_pos_laser    delta player
            |> update_status_laser shoot
  in
    { game  |
        state <- newState,
        player <- newPlayer,
        laser <- newLaser
    }

start_game : Bool -> State -> State
start_game start state =
  case state of
    Pause -> if
      | start     -> Play
      | otherwise -> state
    Play  -> state

pause_game : Bool -> State -> State
pause_game pause state =
  case state of
    Pause -> if
      | pause     -> Play
      | otherwise -> state
    Play  -> if
      | pause     -> Pause
      | otherwise -> state

-- UPDATE LASER

update_pos_laser : Time -> Player -> Laser -> Laser
update_pos_laser dt player laser =
  let
    { x_laser, y_laser, status, fired } = laser
    { x, y } = player
  in
  case status of
    Shooting -> if
      | y_laser > 250 ->
          { laser |
              x_laser <- x,
              y_laser <- -215,
              status  <- Ready
          }
      | otherwise -> 
          { laser | y_laser <- y_laser + dt * 5 }
    Ready ->
      { laser | 
          x_laser <- x,
          y_laser <- -215 
      }
    Collision -> { laser | y_laser <- -215 }

update_status_laser : Bool -> Laser -> Laser
update_status_laser shoot laser =
  let {x_laser, y_laser, vx, vy, status, fired} = laser in
  case status of
    Ready -> if
      | shoot -> { laser | status <- Shooting}
      | otherwise -> laser
    Shooting ->  laser     
    Collision -> { laser | status <- Ready}

-- UPDATE PLAYER

update_vel_player : Int -> Player -> Player
update_vel_player x player =
  let
    newVel n =
      if x == 0
        then toFloat n
        else toFloat n * 2
  in
      { player |
          vx <- newVel x
      }

update_move_player : Int -> Player -> Player
update_move_player x player =
  { player |
      move <-
        if  | x > 0 -> MoveR
            | x < 0 -> MoveL
            | otherwise -> player.move
  }

update_pos_player : Time -> Player -> Player
update_pos_player dt player =
  let {x, y, vx, vy, move} = player in
  { player |
      x <- clamp (-areaW/2) (areaW/2) (x + dt * vx)
  }

-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    { state, player, laser, score } = game
    { x, y, vx, move } = player
    { x_laser, y_laser, status, fired } = laser
    w' = toFloat (w - 1)
    h' = toFloat (h - 1)
  in
  E.container w h E.middle
    <| C.collage areaW areaH
        [ C.rect w' h'
          |> C.filled black
        , renderLaser game
        , renderPlayer game
        , renderHowTo game
        , "Space Invaders"
            |> T.fromString
            |> T.color white
            |> T.height 40
            |> T.centered
            |> C.toForm
            |> C.moveY 220
        , userInterface game
        ]

renderHowTo : Game -> C.Form
renderHowTo game =
  let
    { state } = game
    size = 30
  in
  case state of
    Pause ->
      [ "How to Play"
          |> T.fromString
          |> T.color white
          |> T.height (size + 10)
          |> T.centered
          |> C.toForm
          |> C.moveY (size * 2)
      , "Move - Arrow Keys or WASD"
          |> T.fromString
          |> T.color white
          |> T.height size
          |> T.centered
          |> C.toForm
          |> C.moveY (size * 1)
      , "Shoot - Space"
          |> T.fromString
          |> T.color white
          |> T.height size
          |> T.centered
          |> C.toForm
          |> C.moveY (size * 0)
      , "Enter - Start Game"
          |> T.fromString
          |> T.color white
          |> T.height size
          |> T.centered
          |> C.toForm
          |> C.moveY (size * -1)
      ]
        |> C.group
    Play -> [] |> C.group

renderPlayer : Game -> C.Form
renderPlayer game =
  [ C.rect 40 10
    |> C.filled red
  , C.oval 7 15
    |> C.filled red
    |> C.moveY 5
  ]
    |> C.group
    |> C.move (game.player.x, game.player.y)

renderLaser : Game -> C.Form
renderLaser game =
  let
    { state, player, laser, score } = game
    { x_laser, y_laser, status, fired } = laser
  in
  case status of
    Ready -> 
      C.rect 1 15
        |> C.filled black
        |> C.move (x_laser, y_laser)
    _ ->
      C.rect 1 15
        |> C.filled white
        |> C.move (x_laser, y_laser)

userInterface : Game -> C.Form
userInterface game =
  let
    { state, player, laser, score } = game
    { x, y, vx, move } = player
    { x_laser, y_laser, status } = laser
    size = 20
    borderStyle = 
      let ls = C.defaultLine in
        { ls | color <- darkGray, width <- 5 }
  in
  [ C.rect 150 75
    |> C.outlined borderStyle
    |> C.moveY 15
  , score
    |> toString
    |> T.fromString
    |> T.append (T.fromString "Score : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> C.toForm
    |> C.moveY (size * 2)
  , move
    |> toString
    |> T.fromString
    |> T.append (T.fromString "Move : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> C.toForm
    |> C.moveY (size * 1)
  , status
    |> toString
    |> T.fromString
    |> T.append (T.fromString "Fire : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> C.toForm
    |> C.moveY (size * 0)
  ]
    |> C.group
    |> C.move (150, 140)

-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions gameState

gameState : Signal Game
gameState =
  Signal.foldp update_game initGame input

input : Signal Input
input =
  Signal.sampleOn delta playerInput

playerInput : Signal Input
playerInput =
  Signal.map5 Input
    Keyboard.enter
    (Keyboard.isDown 80)
    (.x <~ (Signal.merge Keyboard.arrows Keyboard.wasd))
    Keyboard.space
    delta

delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (Time.fps 30)