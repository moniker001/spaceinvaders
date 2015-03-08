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

type alias Enemy = Object
  { hp     : Float
  , moving : Direction
  }

type Direction = Left | Right

basicEnemy =
  { hp = 10
  , moving = Left
  , lifetime = 0
  , dim = vec 20 20
  , pos = vec -200 100
  , vel = vec 100 0
  , acc = vec 0 0 
  , gfx = F.rect 20 20 |> F.filled purple
  , rem = False
  }

generateEnemies : Enemy -> Float -> List Enemy
generateEnemies enemy num =
  let list = [1..num] in
  map (\x -> { basicEnemy | pos <- vec (-200 + 50 * x) 100 }) list

-- UPDATE

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
             then (V.scale (-1.0) enemy.vel, Right)
             else (enemy.vel, Left)
    Right -> if pastRBound
             then (V.scale (-1.0) enemy.vel, Left)
             else (enemy.vel, Right)