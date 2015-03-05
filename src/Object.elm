module Object where

import Vector (Vector, vec)
import Vector as V
import Graphics.Collage (Form)
import Graphics.Collage as Form

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

stepPosition : Float -> Vector -> Object a -> Vector
stepPosition delta gameDimensions object =
  let (hx, hy) = V.scale (0.5) object.dim
      (hw, hh) = V.scale (0.5) gameDimensions
  in
      V.bound (vec (-hw + hx) (-hh + hy))
              (vec ( hw - hx) ( hh - hy))
              (V.add object.pos (V.scale delta object.vel))