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

type alias Laser = Object
  { dmg   : Float
  }

{- UPDATE --------------------------------------------------------------------}

update : Event -> Vector -> Laser -> Laser
update ((delta, ks, { x, y }) as event) playerPos laser = 
  let lifetime' = laser.lifetime + delta
      pos'      = updateLaserPos event playerPos laser
      rem'      = updateRem laser
  in
  { laser | lifetime <- lifetime'
          , pos      <- pos'
          , rem      <- rem'
          }

updateLaserPos : Event -> Vector -> Laser -> Vector
updateLaserPos ((delta, ks, { x, y }) as event) playerPos laser =
  let (hx, hy) = V.scale (0.5) laser.dim
      (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
  in
  V.bound (vec (-hw + hx) (-hh + hy))
          (vec ( hw - hx) ( hh - hy))
          (V.add laser.pos (V.scale delta laser.vel))

updateRem : Laser -> Bool
updateRem laser =
  let eps = 20
      isOffScreen l = (V.getY l.pos) >= (gHeight / 2) - eps
  in
  if isOffScreen laser
  then True
  else laser.rem

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