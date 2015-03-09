module Player where

import List (member)

import Color (..)
import Enemy (Enemy)
import Enemy
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage (Form)
import Graphics.Collage as F
import Keyboard as K
import Physics
import Object (Object, CollisionType)
import Object
import Vector (Vector, vec, veci)
import Vector as V

{- TYPE DEFINITION -----------------------------------------------------------}

type WeaponType = Regular | Red | Blue | Green

type alias Player = Object
  { hp     : Float
  , energy : Float
  , wpn    : WeaponType
  , wpncd  : Float
  , cd     : Float
  }

regShip = F.toForm (E.image 75 66 "assets/ship-regular.png")
redShip = F.toForm (E.image 75 66 "assets/ship-red.png")
bluShip = F.toForm (E.image 75 66 "assets/ship-blue.png")
greShip = F.toForm (E.image 75 66 "assets/ship-green.png")

{- UPDATE --------------------------------------------------------------------}

update : Event -> Player -> Player
update ((delta, ks, { x, y }) as ev) player =
  player |> updateLifetime ev
         |> updateCd ev
         |> updatePos ev
         |> updateWpn ev

updateLifetime : Event -> Player -> Player
updateLifetime ((delta, ks, { x, y }) as ev) player =
  { player | lifetime <- player.lifetime + delta }

updateCd : Event -> Player -> Player
updateCd ((delta, ks, { x, y }) as ev) player =
  { player | cd <- if (player.cd - delta) >= 0 
                   then (player.cd - delta)
                   else 0 }

resetCd : Player -> Player
resetCd player = { player | cd <- player.wpncd }

updatePos : Event -> Player -> Player
updatePos ((delta, ks, { x, y }) as ev) player =
  let (hpw, hph) = V.scale (0.5) player.dim
      (hgw, hgh) = V.scale (0.5) (vec gWidth gHeight)
      effVel = 
        if (x == 0 || y == 0)
          then player.vel |> V.cross (veci x y) |> V.scale delta
          else player.vel |> V.cross (veci x y) |> V.scale (delta / sqrt 2)
      pos' = V.bound (vec (-hgw + hpw) (-hgh + hph))
                     (vec ( hgw - hpw) ( hgh - hph))
                     (V.add player.pos effVel)
  in
  { player | pos <- pos' }

updateWpn : Event -> Player -> Player
updateWpn ((delta, ks, { x, y }) as ev) player =
  if | member 49 ks -> { player
                       | wpn <- Regular
                       , gfx <- regShip
                       }
     | member 50 ks -> { player
                       | wpn <- Red
                       , gfx <- redShip
                       }
     | member 51 ks -> { player
                       | wpn <- Blue
                       , gfx <- bluShip
                       }
     | member 52 ks -> { player
                       | wpn <- Green
                       , gfx <- greShip
                       }
     | otherwise    -> player

handleCollisions : CollisionType -> Player -> Player
handleCollisions ct player =
  case ct of
    Object.EnemyCollision -> { player | rem <- True }
    _                      -> player