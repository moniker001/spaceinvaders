module Object where

import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage (Form)
import Graphics.Collage as Form
import List (filter)
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
  , rem : Bool
  }

render : Object a -> Form
render object = Form.move object.pos object.gfx

garbageCollect : List (Object a) -> List (Object a)
garbageCollect objects =
  filter (\o -> o.rem == False) objects