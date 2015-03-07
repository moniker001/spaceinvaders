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