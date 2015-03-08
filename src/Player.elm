module Player where

import List (member)

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage (Form)
import Graphics.Collage as F
import Physics
import Object (Object)
import Vector (Vector, vec, veci)
import Vector as V

{- TYPE DEFINITION -----------------------------------------------------------}

type WeaponType = Regular | Red | Blue | Green

type alias Player = Object
  { lives  : Float
  , hp     : Float
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
updateLifetime ((dt, ks, { x, y }) as ev) player =
  { player | lifetime <- player.lifetime + dt }

updateCd : Event -> Player -> Player
updateCd ((dt, ks, { x, y }) as ev) player =
  { player | cd <- if (player.cd - dt) >= 0 then (player.cd - dt) else 0 }

resetCd : Player -> Player
resetCd player = { player | cd <- player.wpncd }

updatePos : Event -> Player -> Player
updatePos ((dt, ks, { x, y }) as ev) player =
  let (hpw, hph) = V.scale (0.5) player.dim
      (hgw, hgh) = V.scale (0.5) (vec gWidth gHeight)
      effVel = if (x == 0 || y == 0)
               then player.vel |> V.cross (veci x y) |> V.scale dt
               else player.vel |> V.cross (veci x y) |> V.scale (dt / sqrt 2)
      pos' = V.bound (vec (-hgw + hpw) (-hgh + hph))
                     (vec ( hgw - hpw) ( hgh - hph))
                     (V.add player.pos effVel)
  in
  { player | pos <- pos' }

updateWpn : Event -> Player -> Player
updateWpn ((dt, ks, { x, y }) as ev) player =
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
