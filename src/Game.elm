module Game where

import Color (..)
import Enemy (Enemy, basicEnemy)
import Enemy
import Event (..)
import Global (..)
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage (Form)
import Graphics.Collage as F
import Keyboard (KeyCode)
import Keyboard
import Laser (Laser, basicLaser)
import Laser
import List (member, (::), map)
import List
import Object (render)
import Object
import Player (Player)
import Player
import Signal (Signal, (<~), (~))
import Signal
import Text (plainText)
import Text as T
import Time (..)
import Vector (Vector, vec)
import Vector
import Window

type State = Play | Pause

type alias Game =
  { runtime : Float
  , state   : State
  , score   : Float
  , player  : Player
  , lasers  : List Laser
  , enemies : List Enemy
  }

initGame : Game
initGame =
  { runtime = 0
  , state   = Play
  , score   = 0
  , player  = Player.initPlayer
  , lasers  = [basicLaser]
  , enemies = [basicEnemy]
  }

-- UPDATE

upGame : Event -> Game -> Game
upGame ((delta, ks, {x , y}) as event) game =
  let state'   = pauseGame (member 80 ks) game.state
      player'  = Player.update event game.player
      lasers'  = map (Laser.update event game.player.pos) game.lasers
      enemies' = map (Enemy.update event) game.enemies
  in case game.state of
    Pause -> { game | state <- state' }
    Play ->  { game | state   <- state'
                    , player  <- player'
                    , lasers  <- lasers'
                    , enemies <- enemies'
                    }

startGame : Bool -> State -> State
startGame start state =
  case state of
    Pause -> if
      | start     -> Play
      | otherwise -> state
    Play  -> state

pauseGame : Bool -> State -> State
pauseGame pause state =
  case state of
    Pause -> if
      | pause     -> Play
      | otherwise -> state
    Play  -> if
      | pause     -> Pause
      | otherwise -> state

-- SIGNALS

delta : Signal Time
delta = inSeconds <~ fps 60

sigEvent : Signal Event
sigEvent = ((\t l a -> (t, l, a))
           <~ delta ~ Keyboard.keysDown ~ Keyboard.arrows)

sigGame : Signal Game
sigGame = Signal.foldp upGame initGame sigEvent

-- RENDERING

renderGame : Game -> Form
renderGame game =
  let fPlayer = render game.player
      fLasers = F.group (map render game.lasers)
      fEnemies = F.group (map render game.enemies)
      fUI = userInterface game
      pauseScreen = case game.state of
        Play -> E.empty |> F.toForm
        Pause -> "Paused" 
          |> T.fromString 
          |> T.color white 
          |> T.height 30 
          |> T.centered 
          |> F.toForm
  in
  F.group [fUI, fLasers, fPlayer, fEnemies, pauseScreen]

userInterface : Game -> Form
userInterface game =
  let
    { runtime, state, score, player, lasers, enemies } = game
    size = 20
    borderStyle =
      let ls = F.defaultLine in
        { ls | color <- darkGray, width <- 5 }
  in
  [ F.rect 120 80
    |> F.outlined borderStyle
    |> F.moveY 17
  , score
    |> toString
    |> T.fromString
    |> T.append (T.fromString "Score : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> F.toForm
    |> F.moveY (size * 2)
  , player.lives
    |> toString
    |> T.fromString
    |> T.append (T.fromString "Lives : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> F.toForm
    |> F.moveY (size * 1)
  , player.hp
    |> toString
    |> T.fromString
    |> T.append (T.fromString "Health : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> F.toForm
    |> F.moveY (size * 0)
  ]
    |> F.group
    |> F.move (gWidth / 2 + 60, 140)

view : (Int, Int) -> Game -> Element
view (w, h) game =
  let
    w' = toFloat (w - 1)
    h' = toFloat (h - 1)
    bg = F.filled black (F.rect w' h')
    title = "Space Invaders"
              |> T.fromString
              |> T.color white
              |> T.height 40
              |> T.centered
              |> F.toForm
              |> F.moveY 220
  in
  F.collage w h [bg, title, renderGame game]

main : Signal Element
main = view <~ Window.dimensions ~ sigGame
