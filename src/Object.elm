module Object where

import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage (Form)
import Graphics.Collage as Form
import Vector (Vector, vec, vecI)
import Vector as V

type alias Object ext =
  { ext
  | lifetime : Float
  , dim : Vector
  , pos : Vector
  , vel : Vector
  , acc : Vector
  , gfx : Form
  }

render : Object a -> Form
render object = Form.move object.pos object.gfx

stepPosition : Event -> Object a -> Vector
stepPosition ((d, ks, { x, y }) as event) object =
  let (hx, hy) = V.scale (0.5) object.dim
      (hw, hh) = V.scale (0.5) (vec gWidth gHeight)
      effectiveVel =
        if (x == 0 || y == 0)
        then object.vel |> V.cross (vecI x y) |> V.scale d
        else object.vel |> V.cross (vecI x y) |> V.scale (d / sqrt 2)
  in
      V.bound (vec (-hw + hx) (-hh + hy))
              (vec ( hw - hx) ( hh - hy))
              (V.add object.pos effectiveVel)