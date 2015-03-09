module Laser where

import Color (..)
import Enemy (Enemy)
import Enemy
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import List ((::))
import List
import Object (Object, CollisionType)
import Object
import Physics
import Time (Time)
import Vector (Vector, vec)
import Vector as V

{- TYPE DEFINITION -----------------------------------------------------------}

type DmgType = DmgRegular
             | DmgRed
             | DmgBlue
             | DmgGreen

type alias Laser = Object
  { dmg     : Float
  , dmgtype : DmgType
  }

{- UPDATE --------------------------------------------------------------------}

update : Event -> Vector -> Laser -> Laser
update ((dt, ks, { x, y }) as ev) playerPos laser = 
  laser |> updateLifetime ev
        |> updatePos ev playerPos
        |> updateRem

updateLifetime : Event -> Laser -> Laser
updateLifetime ((dt, ks, { x, y }) as ev) laser =
  { laser | lifetime <- laser.lifetime + dt }

updatePos : Event -> Vector -> Laser -> Laser
updatePos ((dt, ks, { x, y }) as event) playerPos laser =
  let (hlw, hlh) = V.scale (0.5) laser.dim
      (hgw, hgh) = V.scale (0.5) (vec gWidth gHeight)
      pos'       = V.bound (vec (-hgw + hlw) (-hgh + hlh))
                           (vec ( hgw - hlw) ( hgh - hlh))
                           (V.add laser.pos (V.scale dt laser.vel))
  in
  { laser | pos <- pos' }

updateRem : Laser -> Laser
updateRem laser =
  let eps = 20
      rem' = if (V.getY laser.pos) >= (gHeight / 2) - eps
             then True
             else laser.rem
  in
  { laser | rem <- rem' }

handleCollision : CollisionType -> Laser -> Laser
handleCollision ct laser =
  case ct of
    Object.EnemyCollision -> { laser | rem <- True }
    _                     -> laser

handleCollisions : List CollisionType -> List Laser -> List Laser
handleCollisions collisions lasers =
  case (collisions, lasers) of
    ([], []) -> []
    (c::cs, l::ls) -> (handleCollision c l)::(handleCollisions cs ls)