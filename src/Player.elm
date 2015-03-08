module Player where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import Physics
import Object (Object)
import Vector (Vector, vec, vecI)
import Vector as V

type alias Player = Object
  { lives  : Float
  , hp     : Float
  , energy : Float
  }

update : Event -> Player -> Player
update ((delta, ks, { x, y }) as event) player =
  let pos' = updatePlayerPos event player
  in
  { player | pos <- pos'
           }

updatePlayerPos : Event -> Object a -> Vector
updatePlayerPos ((d, ks, { x, y }) as event) object =
  let (hx, hy) = V.scale (0.5) object.dim
      (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
      effVel = if (x == 0 || y == 0)
               then object.vel |> V.cross (vecI x y) |> V.scale d
               else object.vel |> V.cross (vecI x y) |> V.scale (d / sqrt 2)
  in
      V.bound (vec (-hw + hx) (-hh + hy))
              (vec ( hw - hx) ( hh - hy))
              (V.add object.pos effVel)

initPlayer =
  { lives = 3
  , hp = 10
  , energy = 10
  , lifetime = 0
  , dim = vec 40 10
  , pos = startPos
  , vel = vec 300 300
  , acc = vec 0 0
  , gfx = [ F.rect 40 10 |> F.filled red
          , F.oval 7  15 |> F.filled red |> F.moveY 5
          ] |> F.group
  , rem = False
  }