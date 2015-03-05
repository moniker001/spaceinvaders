module Laser where

import Color (..)
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

type LaserStatus = Ready | Shooting | Collision

type alias Laser = Object
  { dmg    : Float
  , status : LaserStatus
  }

basicLaser : Laser
basicLaser =
  { dmg = 2
  , lifetime = 0
  , dim = vec 15 15
  , pos = startPos
  , vel = vec 0 200
  , acc = vec 0 0 
  , gfx = F.rect 5 20 |> F.filled blue |> F.moveY 10
  , status = Ready
  }

-- UPDATE

update : Event -> Vector -> Laser -> Laser
update ((delta, ks, { x, y }) as event) playerPos laser = 
  let
    status' = updateLaserStatus event playerPos laser
    pos' = updateLaserPos event playerPos laser
  in
  { laser | status <- status'
          , pos    <- pos'
          }

updateLaserPos : Event -> Vector -> Laser -> Vector
updateLaserPos ((delta, ks, { x, y }) as event) playerPos laser =
  case laser.status of
    Ready -> playerPos
    Shooting -> 
      let (refresh, pos) = refreshLaser event playerPos laser in if
        | refresh -> playerPos
        | otherwise -> pos
    Collision -> playerPos

updateLaserStatus : Event -> Vector -> Laser -> LaserStatus
updateLaserStatus ((delta, ks, { x, y }) as event) playerPos laser =
  case laser.status of
    Ready -> if
      | (member 32 ks) -> Shooting
      | otherwise -> Ready
    Shooting -> if
        | fst (refreshLaser event playerPos laser) -> Ready
        | otherwise -> Shooting
    Collision -> Ready

refreshLaser : Event -> Vector -> Laser -> (Bool, Vector)
refreshLaser ((delta, ks, { x, y }) as event) playerPos laser =
  let
    pos = laser.vel |> V.scale delta |> V.add laser.pos
    refresh = (V.getY pos) > (gHeight / 2)
  in (refresh, pos)