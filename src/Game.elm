module Game where

import Player
import Enemy
import Laser

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

-- GLOBALS

areaW = 500
areaH = 500

type State = Play | Pause

type alias Game =
  { state   : State
  , score   : Float
  , player  : Player
  , laser   : Laser
  , enemies : List Enemy
  }

initGame : Game
initGame =
  let enemy = { x=0, y=0, vx=2, vy=0, status=Hostile } in
  { state  = Pause
  , score  = 0
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
  , enemies = 
    [ enemy
    ]
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
    {state, player, laser, score, enemies} = game
    newState = state
      |> start_game start
      |> pause_game pause
    newPlayer = case state of
      Pause -> player
      Play  -> 
        player
          |> update_vel_player   playerDir
          |> update_move_player  playerDir
          |> update_pos_player   delta
    newLaser = case state of
      Pause -> laser
      Play  -> 
        laser
          |> update_pos_laser    delta player
          |> update_status_laser shoot (List.head enemies)
    newEnemies = case state of
      Pause -> enemies
      Play  ->
        List.map (\x -> update_enemy x game delta) enemies
  in
    { game  |
        state  <- newState,
        player <- newPlayer,
        laser  <- newLaser,
        enemies  <- newEnemies
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

--

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    { state, player, laser, score, enemies } = game
    w' = toFloat (w - 1)
    h' = toFloat (h - 1)
  in
  E.container w h E.middle
    <| C.collage areaW areaH
        [ C.rect w' h'
          |> C.filled black
        , renderLaser laser
        , renderPlayer player
        , renderEnemy (List.head enemies)
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
      , "Start - Enter"
          |> T.fromString
          |> T.color white
          |> T.height size
          |> T.centered
          |> C.toForm
          |> C.moveY (size * -1)
      ]
        |> C.group
    Play -> [] |> C.group



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