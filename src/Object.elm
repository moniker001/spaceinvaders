module Object where

import Global (..)
import Graphics.Collage (Form)
import Graphics.Collage as Form
import Vector (Vector, vec)
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
      effectiveVel = player.vel |> V.cross (vec x y) |> V.scale delta
  in
      V.bound (vec (-hw + hx) (-hh + hy))
              (vec ( hw - hx) ( hh - hy))
              (V.add object.pos effectiveVel)