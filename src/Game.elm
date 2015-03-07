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
import Object (Object, render)
import Object
import Physics
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

garbageCollect : List (Object a) -> List (Object a)
garbageCollect objects =
  List.filter (\o -> o.rem == False) objects

upGame : Event -> Game -> Game
upGame ((delta, ks, {x , y}) as event) game =
  let player'  = Player.update event game.player
      lasers'  = map (Laser.update event player'.pos) game.lasers
                 |> garbageCollect
      enemies' = map (Enemy.update event) game.enemies
                 |> garbageCollect
      (l', e') = handleCollisions lasers' enemies'
      l        = basicLaser
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
                           , lasers  <- l'
                           , enemies <- e'
                           }
       | (member 32 ks) ->
         { game
         | player  <- player'
         , lasers  <- if | allShooting l' ->
                           { l | pos <- game.player.pos }::l'
                         |  otherwise   -> l'
         , enemies <- enemies'
                           }
       | otherwise      -> { game
                           | player  <- player'
                           , lasers  <- l'
                           , enemies <- e'
                           }

handleCollisions : List Laser -> List Enemy -> (List Laser, List Enemy)
handleCollisions lasers enemies =
  (map (\l -> handleCollisionsL l enemies) lasers,
   map (\e -> handleCollisionsE e lasers) enemies)

handleCollisionsL : Laser -> List Enemy -> Laser
handleCollisionsL laser enemies = case enemies of
  [] -> laser
  h::t -> if Physics.isColliding laser.pos laser.dim h.pos h.dim
          then { laser | rem <- True }
          else handleCollisionsL laser t

handleCollisionsE : Enemy -> List Laser -> Enemy
handleCollisionsE enemy lasers = case lasers of
  [] -> enemy
  h::t -> if Physics.isColliding enemy.pos enemy.dim h.pos h.dim
          then { enemy | rem <- True }
          else handleCollisionsE enemy t




allShooting lasers = case lasers of
  []   -> True
  h::t -> if h.state /= Laser.Shooting then False else allShooting t 

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
