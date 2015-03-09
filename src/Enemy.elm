module Enemy where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
<<<<<<< HEAD
import List ((::))
=======
import List (map, member, (::))
import List
>>>>>>> aade1a592c4667486eb83af09b9ebed6f4ecc405
import Physics
import Object (Object, CollisionType)
import Object
import Vector (Vector, vec)
import Vector as V

{- TYPE DEFINITION -----------------------------------------------------------}

type alias Enemy = Object
  { hp     : Float
  , moving : Direction
  , reach  : Bool
  }

<<<<<<< HEAD
type Direction = Left | Right | Down (Direction, Float)
=======
type Direction = Left | Right | Down
>>>>>>> aade1a592c4667486eb83af09b9ebed6f4ecc405

{- UPDATE --------------------------------------------------------------------}

update : Event -> Enemy -> Enemy
<<<<<<< HEAD
update ((delta, ks, { x, y }) as ev) enemy =
  enemy |> updatePos ev
        |> updateVel ev

updatePos : Event -> Enemy -> Enemy
updatePos ((dt, ks, { x, y }) as ev) enemy =
  let (hew, heh) = V.scale (0.5) enemy.dim
      (hgw, hgh) = V.scale (0.5) (vec gWidth gHeight)
      effVel     = case enemy.moving of
                     Left   -> V.cross (vec 1  0) (V.scale dt enemy.vel)
                     Right  -> V.cross (vec 1  0) (V.scale dt enemy.vel)
                     Down _ -> V.cross (vec 0 -1) (V.scale dt enemy.vel)
      pos'       = V.bound (vec (-hgw + hew) (-hgh + heh))
                           (vec ( hgw - hew) ( hgh - heh))
                           (V.add enemy.pos effVel)
  in
  { enemy | pos <- pos' }

updateVel : Event -> Enemy -> Enemy
updateVel ((dt, ks, { x, y }) as ev) enemy =
  let ex = V.getX enemy.pos
      ey = V.getY enemy.pos
      (hew, heh) = V.scale (0.5) enemy.dim
      (hgw, hgh) = V.scale (0.5) (vec gWidth gHeight)
      eps = 20
      pastLBound = (ex >=  hgw - hew - eps)
      pastRBound = (ex <= -hgw + hew + eps)
      nextDBound = (V.getY enemy.pos) - 50
  in
  case (enemy.moving) of
    Left        -> if pastLBound
                   then { enemy | moving <- Down (Right, nextDBound)
                                }
                   else enemy
    Right       -> if pastRBound
                   then { enemy | moving <- Down (Left, nextDBound)
                                }
                   else enemy
    Down (d, b) -> if (ey <= b)
                   then { enemy | vel    <- V.cross (vec -1 1) enemy.vel
                                , moving <- d
                                }
                   else enemy
=======
update event enemy =
  let pos' = updateEnemyPos event enemy
      (vel', moving') = updateEnemyVel event enemy
      reach' = updateEnemyReach pos'
  in
  { enemy | pos    <- pos'
          , vel    <- vel'
          , moving <- moving'
          , reach  <- reach'
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

updateEnemyReach : Vector -> Bool
updateEnemyReach (x, y) =
  let
    eps = 50
    lowerBound = -gHHeight + 50
  in
  if (y <= lowerBound) then True else False
>>>>>>> aade1a592c4667486eb83af09b9ebed6f4ecc405

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