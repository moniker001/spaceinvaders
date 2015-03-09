module Enemy where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import List (map, member, (::))
import List
import Physics
import Object (Object, CollisionType)
import Object
import Vector (Vector, vec)
import Vector as V

{- TYPE DEFINITION -----------------------------------------------------------}

type alias Enemy = Object
  { hp     : Float
  , moving : Direction
  }

type Direction = Left | Right | Down

{- UPDATE --------------------------------------------------------------------}

update : Event -> Enemy -> Enemy
update event enemy =
  let pos' = updateEnemyPos event enemy
      (vel', moving') = updateEnemyVel event enemy
  in
  { enemy | pos    <- pos'
          , vel    <- vel'
          , moving <- moving'
          }

updateEnemyPos : Event -> Enemy -> Vector
updateEnemyPos ((delta, ks, { x, y }) as event) enemy =
  let
    (hx, hy) = V.scale (0.5) enemy.dim
    (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
    lowerBound = vec (-hw + hx) (-hh + hy)
    upperBound = vec ( hw - hx) ( hh - hy)
  in
    V.bound lowerBound
            upperBound
            (V.add enemy.pos (V.scale delta enemy.vel))

updateEnemyVel : Event -> Enemy -> (Vector, Direction)
updateEnemyVel event enemy =
  let (ex, ey) = enemy.pos
      (hx, hy) = V.scale (0.5) enemy.dim
      (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
      eps = 10
      pastLBound = (ex >= hw - hx - eps)
      pastRBound = (ex <= -hw + hx + eps)
      leftVel = vec -100 0
      rightVel = vec 100 0
      downVel = vec 0 -100
  in
  case (enemy.moving) of
    Left  -> if pastLBound
             then (V.cross (-1, 1) enemy.vel, Right)
             else (enemy.vel, Left)
    Right -> if pastRBound
             then (V.cross (-1, 1) enemy.vel, Left)
             else (enemy.vel, Right)

handleCollision : CollisionType -> Enemy -> Enemy
handleCollision ct enemy =
  case ct of
    Object.LaserCollision -> { enemy | rem <- True }
    _                     -> enemy

handleCollisions : List CollisionType -> List Enemy -> List Enemy
handleCollisions collisions enemies =
  case (collisions, enemies) of
    ([], []) -> []
    (c::cs, e::es) -> (handleCollision c e)::(handleCollisions cs es)