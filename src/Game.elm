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
import Random (Seed)
import Random
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
import Object (Object)
import Object
import Physics
import Player (Player)
import Player
import Vector (Vector, vec)
import Vector


{- TYPE DEFINITION -----------------------------------------------------------}

type GameState = Playing | Paused | GameOver

type alias Game =
  { runtime : Float
  , state   : GameState
  , score   : Float
  , lives   : Float
  , player  : Player
  , lasers  : List Laser
  , enemies : List Enemy
  }

{- INSTANCES -----------------------------------------------------------------}

initPlayer : Player
initPlayer =
  { hp       = 10
  , energy   = 10
  , lifetime = 0
  , objtype  = Object.Player
  , dim      = vec 75 66
  , pos      = startPos
  , vel      = vec 150 150
  , acc      = vec 0 0
  , gfx      = F.toForm (E.image 75 66 "assets/ship-regular.png")
  , rem      = False
  , wpn      = Player.Regular
  , wpncd    = 1
  , cd       = 0
  }

initEnemy : Enemy
initEnemy =
  { hp        = 10
  , moving    = Enemy.Left
  , reach     = False
  , enemytype = Enemy.Regular
  , lifetime  = 0
  , objtype   = Object.Enemy
  , dim       = vec 39 39
  , pos       = vec -200 100
  , vel       = vec 150 150
  , acc       = vec 0 0 
  , gfx       = F.toForm (E.image 39 39 "assets/enemy-regular.png")
  , rem       = False
  }

redEnemy : Enemy
redEnemy = { initEnemy
           | gfx <- F.toForm (E.image 39 39 "assets/enemy-red.png")
           , enemytype <- Enemy.Red
           }

bluEnemy : Enemy
bluEnemy = { initEnemy
           | gfx       <- F.toForm (E.image 39 39 "assets/enemy-blue.png")
           , enemytype <- Enemy.Blue
           }

greEnemy : Enemy
greEnemy = { initEnemy
           | gfx       <- F.toForm (E.image 39 39 "assets/enemy-green.png")
           , enemytype <- Enemy.Green
           }

basLaser : Laser
basLaser =
  { dmg      = 2
  , dmgtype  = Laser.DmgRegular
  , source   = Laser.SourcePlayer
  , lifetime = 0
  , objtype  = Object.Laser
  , dim      = vec 10 10
  , pos      = startPos
  , vel      = vec 0 400
  , acc      = vec 0 0 
  , gfx      = F.rect 10 10 |> F.filled grey
  , rem      = False
  }

redLaser : Laser
redLaser = { basLaser | gfx <- F.rect 10 10 |> F.filled red
                      , dmgtype <- Laser.DmgRed
                      }

bluLaser : Laser
bluLaser = { basLaser | gfx <- F.rect 10 10 |> F.filled blue
                      , dmgtype <- Laser.DmgBlue
                      }

greLaser : Laser
greLaser = { basLaser | gfx <- F.rect 10 10 |> F.filled green
                      , dmgtype <- Laser.DmgGreen
                      }

initGame : Game
initGame =
  { lives   = 3
  , runtime = 0
  , state   = Playing
  , score   = 0
  , player  = initPlayer
  , lasers  = []
  , enemies = generateEnemies [1..10] 250 (Random.initialSeed 1)
  }

initEarth : Object {}
initEarth =
  { lifetime = 0
  , objtype  = Object.None
  , dim      = vec 800 74
  , pos      = vec 0 (-gHeight/2 + 37)
  , vel      = vec 0 0
  , acc      = vec 0 0 
  , gfx      = F.toForm (E.image 800 74 "assets/earth.png")
  , rem      = False
  }

generateEnemies : (List Float) -> Float -> Seed -> List Enemy
generateEnemies num ypos seed =
  case num of
    []   -> []
    h::t -> let (enemy', seed') = generateRandomEnemy
                                  (vec (-400 + 75 * h) ypos)
                                  seed
            in
            enemy' :: (generateEnemies t ypos seed')

generateRandomEnemy : Vector -> Seed -> (Enemy, Seed)
generateRandomEnemy pos seed =
  let (rnum, rseed) = Random.generate (Random.int 0 3) seed
  in
  ((generateEnemy pos rnum), rseed)

generateEnemy : Vector -> Int -> Enemy
generateEnemy pos enemytype =
  case enemytype of
    0 -> { initEnemy | pos <- pos }
    1 -> { redEnemy  | pos <- pos }
    2 -> { bluEnemy  | pos <- pos }
    3 -> { greEnemy  | pos <- pos }
    _ -> { initEnemy | pos <- pos }


{- UPDATE --------------------------------------------------------------------}

update : Event -> Game -> Game
update ((delta, ks, { x, y }) as ev) game =
  let game' = game |> updatePlayer ev
                   |> updateLasers ev
                   |> remEnemies
                   |> updateEnemies ev
                   |> updateLives
                   |> remLasers
                   |> playerDeath
  in
  case game.state of
    Paused -> if | (member 79 ks) -> { game | state <- Playing }
                 | otherwise      -> game

    Playing ->
    if | (member 80 ks) -> { game' | state  <- Paused }
       | (member 32 ks) && (game'.player.cd == 0) ->
         let new = selectWeapon game
         in
         { game' | lasers <- new :: game'.lasers
                 , player <- Player.resetCd game'.player
                 }
       | otherwise -> game'
    GameOver ->
    if | (member 13 ks) -> initGame
       | otherwise -> game

selectWeapon : Game -> Laser
selectWeapon game =
  case game.player.wpn of
    Player.Regular -> { basLaser | pos <- game.player.pos }
    Player.Red     -> { redLaser | pos <- game.player.pos }
    Player.Green   -> { greLaser | pos <- game.player.pos }
    Player.Blue    -> { bluLaser | pos <- game.player.pos }

updateLives : Game -> Game
updateLives game =
  let list = List.filter (\e -> e.reach) game.enemies
      len = toFloat (List.length list)
      lives' = game.lives - len
  in
  if | (lives' <= 0) -> { game | lives <- 0, state <- GameOver }
     | otherwise -> { game | lives <- lives'}

updatePlayer : Event -> Game -> Game
updatePlayer ev game =
  let playerCollisions = Object.checkCollision game.player game.enemies
      player' = game.player
                |> Player.update ev
                |> Player.handleCollisions playerCollisions
  in
  { game
  | player <- Player.update ev player' }

playerDeath : Game -> Game
playerDeath game =
  if (game.player.rem == False) then game else { game | state <- GameOver }

updateEnemies : Event -> Game -> Game
updateEnemies ev game =
  let enemyCollisions = map (\e -> Object.checkCollision e game.lasers)
                            game.enemies
      enemies' = game.enemies |> map (Enemy.update ev)
                              |> Enemy.handleCollisions enemyCollisions
      score' = updateScore game.enemies enemies'
  in
  { game | enemies <- enemies'
         , score   <- game.score + score'}

updateScore : List Enemy -> List Enemy -> Float
updateScore oldEnemies newEnemies =
  let numOld        = toFloat (List.length oldEnemies)
      numNew        = toFloat (List.length newEnemies)
      kills         = numOld - numNew
      pointsperkill = 10
  in
  if (kills >= 0) then kills * pointsperkill else 0

updateLasers : Event -> Game -> Game
updateLasers ev game =
  let enemyCollisions = map (\l -> Object.checkCollision l game.enemies)
                            game.lasers
      lasers' = game.lasers
        |> map (Laser.update ev game.player.pos)
        |> Laser.handleCollisions enemyCollisions
  in
  { game | lasers <- lasers' }

remLasers : Game -> Game
remLasers game =
  { game | lasers <- Object.garbageCollect game.lasers }

remEnemies : Game -> Game
remEnemies game =
  { game | enemies <- Object.garbageCollect game.enemies }

{- SIGNALS -------------------------------------------------------------------}

sDelta : Signal Time
sDelta = inSeconds <~ fps 120

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
  let fPlayer         = Object.render game.player
      fLasers         = F.group (map Object.render game.lasers)
      fEnemies        = F.group (map Object.render game.enemies)
      fUserInterface  = userInterface game
      fDebugInterface = debugInterface game
      fEarth          = Object.render initEarth
      pauseScreen = case game.state of
        Playing -> E.empty |> F.toForm
        Paused  -> "Paused - Press P to resume"
                    |> renderString white 30 (0,0) 
        GameOver -> "Game Over - Press Enter to play again!"
                    |> renderString white 30 (0, 0)
  in
  F.group [ fLasers
          , fEarth
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
  [ game.score        |> textstyle "SCORE: "       2
  , game.lives |> textstyle "LIVES: "       1
  , game.player.hp    |> textstyle "HEALTH: "      0
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
      enemyVel = case game.enemies of
        [] -> (0,0)
        h::_ -> ((fst h.vel), (snd h.vel))
      enemyRem = case game.enemies of
        [] -> Nothing
        h::_ -> Just h.rem
      laserRem = case game.lasers of
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
  [ game.runtime             |> textstyle "RUNTIME: "     7
  , enemyRem                 |> textstyle "ENEMY REM: "   6
  , laserRem                 |> textstyle "LASER REM: "   5
  , enemyVel                 |> textstyle "ENEMY VEL: "   4
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