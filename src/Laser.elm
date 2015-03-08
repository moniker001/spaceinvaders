module Laser where

import Color (..)
import Enemy (Enemy)
import Enemy
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import List (member)
import Object (Object)
import Physics
import Time (Time)
import Vector (Vector, vec, vecI)
import Vector as V

type LaserState = Ready | Shooting

type alias Laser = Object
  { dmg   : Float
  , cd    : Float
  , state : LaserState
  }

type LaserEvent = OffScreen | Collision

handleLaserEvent : LaserEvent -> Laser -> Laser
handleLaserEvent event laser =
  case (event, laser.state) of
    (OffScreen, _) -> { laser | rem <- True }
    (Collision, _) -> { laser | rem <- True }
    _              -> laser

checkOffScreen : Laser -> Bool
checkOffScreen laser = 
  let eps = 20
  in
  (V.getY laser.pos) >= (gHeight / 2) - eps

-- UPDATE

update : Event -> Vector -> List Enemy -> Laser -> Laser
update ((delta, ks, { x, y }) as event) playerPos enemies laser = 
  let lifetime' = laser.lifetime + delta
      state' = updateLaserState event playerPos laser
      pos'   = updateLaserPos event playerPos laser
      rem'   = updateLaserRem laser enemies
  in
  { laser | lifetime <- lifetime'
          , state    <- state'
          , pos      <- pos'
          , rem      <- rem'
          }

checkLaserCollision : Laser -> List Enemy -> Bool
checkLaserCollision laser enemies = case enemies of
  [] -> False
  h::t -> if 
    | Physics.isColliding laser.pos laser.dim h.pos h.dim -> True
    | otherwise -> checkLaserCollision laser t

updateLaserRem : Laser -> List Enemy -> Bool
updateLaserRem laser enemies =
  let offscreen = checkOffScreen laser in
  if offscreen then True else (checkLaserCollision laser enemies)

updateLaserPos : Event -> Vector -> Laser -> Vector
updateLaserPos ((delta, ks, { x, y }) as event) playerPos laser =
  case laser.state of
    Ready    -> playerPos
    Shooting ->
      let (hx, hy) = V.scale (0.5) laser.dim
          (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
      in
      V.bound (vec (-hw + hx) (-hh + hy))
              (vec ( hw - hx) ( hh - hy))
              (V.add laser.pos (V.scale delta laser.vel))

updateLaserState : Event -> Vector -> Laser -> LaserState
updateLaserState ((delta, ks, { x, y }) as event) playerPos laser =
  case laser.state of
    Ready -> if | (member 32 ks && laser.lifetime > laser.cd) -> Shooting
                | otherwise -> Ready
    Shooting -> Shooting

-- INSTANCES

basicLaser : Laser
basicLaser =
  { dmg = 2
  , cd = 0.5
  , state = Ready
  , lifetime = 0
  , dim = vec 5 20
  , pos = startPos
  , vel = vec 0 300
  , acc = vec 0 0 
  , gfx = F.rect 5 20 |> F.filled blue
  , rem = False
  }