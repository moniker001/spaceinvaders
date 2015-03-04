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

type EnemyStatus = Hostile | Dead

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

type alias Enemy =
  { x      : Float
  , y      : Float
  , vx     : Float
  , vy     : Float
  , status : EnemyStatus
  }

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

update_status_laser : Bool -> Enemy -> Laser -> Laser
update_status_laser shoot enemy laser =
  let {x_laser, y_laser, vx, vy, status, fired} = laser in
  case status of
    Ready -> if
      | shoot -> { laser | status <- Shooting}
      | otherwise -> laser
    Shooting ->  { laser | status <- check_collision laser enemy }     
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

-- UPDATE ENEMY

update_vel_enemy : Int -> Enemy -> Enemy
update_vel_enemy vx enemy =
  let
    {x, y, vx, vy, status} = enemy
  in if
    | x <= -areaW / 2 -> { enemy | vx <- -vx }
    | x >= areaW / 2 -> { enemy | vx <- -vx }
    | otherwise -> { enemy | vx <- vx }

update_pos_enemy : Time -> Enemy -> Enemy
update_pos_enemy dt enemy =
  let {x, y, vx, vy} = enemy in
  { enemy |
      x <- (x + dt * vx),
      y <- (y + dt * vy)
  }

check_collision : Laser -> Enemy -> LaserStatus
check_collision laser enemy =
  let
    {x_laser, y_laser, status} = laser
    {x, y} = enemy
  in
  case status of
    Ready -> Ready
    Shooting -> if
      | (abs (x_laser - x)) < 10 && (abs (y_laser - y)) < 10 -> Collision
      | otherwise -> Shooting
    Collision -> Collision

update_status_enemy : Laser -> Enemy -> Enemy
update_status_enemy laser enemy =
  case (check_collision laser enemy) of
    Collision -> { enemy | status <- Dead }
    _         -> enemy

update_enemy : Enemy -> Game -> Time -> Enemy
update_enemy enemy game dt =
  let
    {state, player, laser, score, enemies} = game
    {x_laser, y_laser, status} = laser
    collision = check_collision laser enemy
  in enemy
    |> update_pos_enemy dt
    |> update_vel_enemy 20
    |> update_status_enemy laser

-- VIEW

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

renderPlayer : Player -> C.Form
renderPlayer player =
  [ C.rect 40 10
    |> C.filled red
  , C.oval 7 15
    |> C.filled red
    |> C.moveY 5
  ]
    |> C.group
    |> C.move (player.x, player.y)

renderLaser : Laser -> C.Form
renderLaser laser =
  case laser.status of
    Ready -> 
      C.rect 1 15
        |> C.filled black
        |> C.move (laser.x_laser, laser.y_laser)
    _ ->
      C.rect 1 15
        |> C.filled white
        |> C.move (laser.x_laser, laser.y_laser)

renderEnemy: Enemy -> C.Form
renderEnemy enemy =
  case enemy.status of
    Hostile ->
      C.rect 20 20
        |> C.filled blue
        |> C.move (enemy.x, enemy.y)
    Dead ->
      E.empty
        |> C.toForm
        |> C.move (enemy.x, enemy.y)

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