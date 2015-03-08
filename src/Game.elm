module Game where

import Color (..)
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage (Form)
import Graphics.Collage as F
import Keyboard (KeyCode)
import Keyboard as K
import List (member, (::), map)
import List
import Signal (Signal, (<~), (~))
import Signal
import Text (plainText)
import Text as T
import Time (..)
import Window

import Enemy (Enemy)
import Enemy
import Event (..)
import Global (..)
import Laser (Laser)
import Laser
import Object (..)
import Physics
import Player (Player)
import Player
import Vector (Vector, vec)
import Vector


{- TYPE DEFINITION -----------------------------------------------------------}

type GameState = Playing | Paused

type alias Game =
  { runtime : Float
  , state   : GameState
  , score   : Float
  , player  : Player
  , lasers  : List Laser
  , enemies : List Enemy
  }

{- INSTANCES -----------------------------------------------------------------}

initPlayer : Player
initPlayer =
  { lives    = 3
  , hp       = 10
  , energy   = 10
  , lifetime = 0
  , dim      = vec 75 66
  , pos      = startPos
  , vel      = vec 300 300
  , acc      = vec 0 0
  , gfx      = F.toForm (E.image 75 66 "assets/ship-regular.png")
  , rem      = False
  , wpn      = Player.Regular
  }

initEnemy : Enemy
initEnemy =
  { hp = 10
  , moving = Enemy.Left
  , lifetime = 0
  , dim = vec 20 20
  , pos = vec -200 100
  , vel = vec 100 -5
  , acc = vec 0 0 
  , gfx = F.rect 20 20 |> F.filled purple
  , rem = False
  }

basicLaser : Laser
basicLaser =
  { dmg      = 2
  , cd       = 0.25
  , state    = Laser.Ready
  , lifetime = 0
  , dim      = vec 5 30
  , pos      = startPos
  , vel      = vec 0 300
  , acc      = vec 0 0 
  , gfx      = F.rect 5 30 |> F.filled blue
  , rem      = False
  }

initGame : Game
initGame =
  { runtime = 0
  , state   = Playing
  , score   = 0
  , player  = initPlayer
  , lasers  = [basicLaser]
  , enemies = [initEnemy]
  }

generateEnemies : Enemy -> Float -> List Enemy
generateEnemies enemy num =
  let list = [1..num] in
  map (\x -> { initEnemy | pos <- vec (-200 + 50 * x) 100 }) list

{- UPDATE --------------------------------------------------------------------}

update ((delta, ks, {x , y}) as event) game =
  let 
    newPlayer = 
      Player.update event game.player
    lasers = 
      map (Laser.update event newPlayer.pos game.enemies) game.lasers
    enemies = 
      map (Enemy.update event) game.enemies
    newLasers = 
      garbageCollect lasers
    enemies' = 
      garbageCollect (handleCollisions game.lasers game.enemies)
    newEnemies = 
      map (Enemy.update event) enemies'
    newScore =
      game.score + (updateScore game.enemies newEnemies)
    l = basicLaser
  in
  case game.state of
    -- if the game is paused
    Paused ->
    if | (member 80 ks) -> { game | state <- Playing }
       | otherwise      -> game
    -- if the game is playing
    Playing  -> 
    if | (member 80 ks) -> { game | state <- Paused }
       | (member 32 ks) ->
         { game
         | player  <- newPlayer
         , score   <- newScore
         , lasers  <- if
           | allShooting newLasers ->
               newLasers++[{ l | pos <- game.player.pos }]
           | otherwise -> newLasers
         , enemies <- newEnemies
         }
       | otherwise ->
         { game
         | player  <- newPlayer
         , score   <- newScore
         , lasers  <- newLasers
         , enemies <- newEnemies
         }

updateScore : List Enemy -> List Enemy -> Float
updateScore oldEnemies newEnemies =
  let
    numOld = toFloat (List.length oldEnemies)
    numNew = toFloat (List.length newEnemies)
    kills  = numOld - numNew
    points = 10
  in
  if (kills >= 0) then kills * points else 0

handleCollisions : List Laser -> List Enemy -> List Enemy
handleCollisions lasers enemies =
  (map (\e -> handleCollisionsE e lasers) enemies)

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

allShooting : List Laser -> Bool
allShooting lasers = case lasers of
  []   -> True
  h::t -> if h.state /= Laser.Shooting then False else allShooting t 

{- SIGNALS -------------------------------------------------------------------}

sDelta : Signal Time
sDelta = inSeconds <~ fps 30

sEvent : Signal Event
sEvent = ((\t l a -> (t, l, a)) <~ sDelta ~ K.keysDown ~ K.arrows)

sGame : Signal Game
sGame = Signal.foldp update initGame sEvent

{- RENDER --------------------------------------------------------------------}

renderString : Color -> Float -> (Float, Float) -> String -> Form
renderString color height (x, y) string =
  string |> T.fromString
         |> T.color color
         |> T.height height
         |> T.centered
         |> F.toForm
         |> F.move (x,y)

renderGame : Game -> Form
renderGame game =
  let fPlayer = render game.player
      fLasers = F.group (map render game.lasers)
      fEnemies = F.group (map render game.enemies)
      fUserInterface = userInterface game
      fDebugInterface = debugInterface game
      pauseScreen = case game.state of
        Playing -> E.empty |> F.toForm
        Paused  -> "Paused - Press P to resume"
                    |> renderString white 30 (0,0) 
  in
  F.group [ fLasers
          , fPlayer
          , fEnemies
          , fUserInterface
          , fDebugInterface
          , pauseScreen
          ]

userInterface : Game -> Form
userInterface game =
  let size = 20
      textstyle prefix index = 
        (\x -> toString x
          |> T.fromString
          |> T.append (T.fromString prefix)
          |> T.color green
          |> T.height size
          |> T.centered
          |> F.toForm
          |> F.moveY (size * index))
  in
  [ game.score               |> textstyle "SCORE: "       2
  , game.player.lives        |> textstyle "LIVES: "       1
  , game.player.hp           |> textstyle "HEALTH: "      0
  ]
    |> F.group
    |> F.move (-gWidth / 2 - 100, 0)

debugInterface : Game -> Form
debugInterface game =
  let size = 20
      enemyPos = case game.enemies of
        [] -> (0,0)
        h::_ -> (floor (fst h.pos), floor (snd h.pos))
      laserPos = case game.lasers of
        [] -> (0,0)
        h::_ -> (floor (fst h.pos), floor (snd h.pos))
      laserrem = case game.lasers of
        [] -> Nothing
        h::_ -> Just h.rem
      textstyle prefix index = 
        (\x -> toString x
          |> T.fromString
          |> T.append (T.fromString prefix)
          |> T.color red
          |> T.height size
          |> T.centered
          |> F.toForm
          |> F.moveY (size * index))
  in
  [ game.runtime             |> textstyle "RUNTIME: "     5
  , laserrem                 |> textstyle "LASER REM: "   4
  , List.length game.enemies |> textstyle "NUM ENEMIES: " 3
  , List.length game.lasers  |> textstyle "NUM LASERS: "  2
  , enemyPos                 |> textstyle "ENEMY: "       1
  , laserPos                 |> textstyle "LASER: "       0
  ]
    |> F.group
    |> F.move (gWidth / 2 + 100, 0)

{- MAIN ----------------------------------------------------------------------}

view : (Int, Int) -> Game -> Element
view (w, h) game =
  let
    bg = F.filled black (F.rect gWidth gHeight)
    title = "Space Invaders"
      |> renderString white 40 (0, gHHeight - 25)
  in
  F.collage w h [bg, renderGame game, title]

main : Signal Element
main = view <~ Window.dimensions ~ sGame
