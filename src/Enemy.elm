module Enemy where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import Physics
import Object (Object)
import Vector (Vector, vec)
import Vector as V

type alias Enemy = Object
  { hp : Float
  }

basicEnemy =
  { hp = 10
  , lifetime = 0
  , dim = vec 20 20
  , pos = vec 0 50
  , vel = vec 50 0
  , acc = vec 0 0 
  , gfx = F.rect 20 20 |> F.filled purple
  }

-- UPDATE

update : Event -> Enemy -> Enemy
update ((delta, ks, { x, y }) as event) enemy =
  let
    pos' = updateEnemyPos event enemy
    vel' = updateEnemyVel event enemy
  in
  { enemy | pos <- pos'
          , vel <- vel'
          }

updateEnemyPos : Event -> Enemy -> Vector
updateEnemyPos ((delta, ks, { x, y }) as event) enemy =
  enemy.vel |> V.scale delta |> V.add enemy.pos

updateEnemyVel : Event -> Enemy -> Vector
updateEnemyVel ((delta, ks, { x, y }) as event) enemy =
  let pos = enemy.vel |> V.scale delta |> V.add enemy.pos in if
    | (fst pos) > (gWidth / 2) || (fst pos) < (-gWidth / 2) ->
        enemy.vel |> V.scale -1
    | otherwise -> enemy.vel