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

type GameState = Playing | Paused

type alias Game =
  { runtime : Float
  , state   : GameState
  , score   : Float
  , player  : Player
  , lasers  : List Laser
  , enemies : List Enemy
  }

initGame : Game
initGame =
  { runtime = 0
  , state   = Playing
  , score   = 0
  , player  = Player.initPlayer
  , lasers  = [basicLaser]
  , enemies = [basicEnemy]
  }

-- UPDATE

upGame : Event -> Game -> Game
upGame ((delta, ks, {x , y}) as event) game =
  let player'  = Player.update event game.player
      lasers'  = map (Laser.update event player'.pos) game.lasers
      enemies' = map (Enemy.update event) game.enemies
  in
  case game.state of
    -- if the game is paused
    Paused ->
    if | (member 80 ks) -> { game | state <- Playing }
       | otherwise      -> game
    -- if the game is playing
    Playing  -> 
    if | (member 80 ks) -> { game
                           | state   <- Paused
                           , player  <- player'
                           , lasers  <- lasers'
                           , enemies <- enemies'
                           }
       | otherwise      -> { game
                           | player  <- player'
                           , lasers  <- lasers'
                           , enemies <- enemies'
                           }

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
      fUserInterface = userInterface game
      pauseScreen = case game.state of
        Playing -> E.empty |> F.toForm
        Paused  -> "Paused - Press P to resume" 
                   |> T.fromString 
                   |> T.color white 
                   |> T.height 30 
                   |> T.centered 
                   |> F.toForm
  in
  F.group [ fLasers
          , fPlayer
          , fEnemies
          , pauseScreen]

userInterface : Game -> Form
userInterface game =
  let size         = 20
  in
  [ game.score |> toString
    |> T.fromString
    |> T.append (T.fromString "SCORE: ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> F.toForm
    |> F.moveY (size * 2)
  , game.player.lives
    |> toString
    |> T.fromString
    |> T.append (T.fromString "LIVES : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> F.toForm
    |> F.moveY (size * 1)
  , game.player.hp
    |> toString
    |> T.fromString
    |> T.append (T.fromString "HEALTH : ")
    |> T.color green
    |> T.height size
    |> T.centered
    |> F.toForm
    |> F.moveY (size * 0)
  , "Space Invaders"
    |> T.fromString
    |> T.color white
    |> T.height 40
    |> T.centered
    |> F.toForm
    |> F.moveY 220
  ]
    |> F.group
    |> F.move (gWidth / 2 + 60, 140)

view : (Int, Int) -> Game -> Element
view (w, h) game =
  let bg = F.filled black (F.rect gWidth gHeight)
  in
  F.collage w h [bg, renderGame game]

main : Signal Element
main = view <~ Window.dimensions ~ sigGame
