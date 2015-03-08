module Enemy where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import List (map)
import List
import Physics
import Object (Object)
import Object
import Vector (Vector, vec)
import Vector as V

{- TYPE DEFINITION -----------------------------------------------------------}

type alias Enemy = Object
  { hp     : Float
  , moving : Direction
  }

type Direction = Left | Right

{- UPDATE --------------------------------------------------------------------}

update : Event -> Enemy -> Enemy
update ((delta, ks, { x, y }) as event) enemy =
  let pos' = updateEnemyPos event enemy
      (vel', moving') = updateEnemyVel event enemy
  in
  { enemy | vel    <- vel'
          , pos    <- pos'
          , moving <- moving'
          }

updateEnemyPos : Event -> Enemy -> Vector
updateEnemyPos ((delta, ks, { x, y }) as event) enemy =
  let (hx, hy) = V.scale (0.5) enemy.dim
      (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
  in
      V.bound (vec (-hw + hx) (-hh + hy))
              (vec ( hw - hx) ( hh - hy))
              (V.add enemy.pos (V.scale delta enemy.vel))

updateEnemyVel : Event -> Enemy -> (Vector, Direction)
updateEnemyVel ((delta, ks, { x, y }) as event) enemy =
  let ex = V.getX enemy.pos
      (hx, hy) = V.scale (0.5) enemy.dim
      (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
      eps = 10
      pastLBound = (ex >= hw - hx - eps)
      pastRBound = (ex <= -hw + hx + eps)
  in
  case (enemy.moving) of
    Left  -> if pastLBound
             then (V.cross (-1, 1) enemy.vel, Right)
             else (enemy.vel, Left)
    Right -> if pastRBound
             then (V.cross (-1, 1) enemy.vel, Left)
             else (enemy.vel, Right)