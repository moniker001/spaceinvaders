module Game where

import Color (..)
import Enemy (Enemy)
import Enemy
import Event (Event)
import Event (..)
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage (Form)
import Graphics.Collage as F
import Keyboard (KeyCode)
import Keyboard
import Laser (Laser)
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

upGame : Event -> Game -> Game
upGame ((delta, ks, {x , y}) as event) game =
  let state'   = pauseGame (member 80 ks) game.state
      player'  = Player.update event game.player
      lasers'  = map (Laser.update event) game.lasers
      enemies' = map (Enemy.update event) game.enemies
  in
  { game | state   <- state'
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

initGame : Game
initGame =
  { runtime = 0
  , state   = Pause
  , score   = 0
  , player  = Player.initPlayer
  , lasers  = []
  , enemies = []
  }

-- SIGNALS

delta : Signal Time
delta = inSeconds <~ fps 60

type alias Event = (Float, List KeyCode, { x : Int, y : Int })

getDelta : Event -> Float
getDelta (delta, keysDown, arrows) = delta

sigEvent : Signal Event
sigEvent = ((\t l a -> (t, l, a))
           <~ delta ~ Keyboard.keysDown ~ Keyboard.arrows)

sigGame : Signal Game
sigGame = Signal.foldp upGame initGame sigEvent

-- Rendering

renderGame : Game -> Form
renderGame game =
  let fPlayer = render game.player
      fLasers = F.group (map render game.lasers)
      fEnemies = F.group (map render game.enemies)
  in
  F.group [fPlayer, fLasers, fEnemies]

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
