module Object where

import Vector (Vector)
import Vector
import Graphics.Collage (Form)
import Graphics.Collage as Form

type alias Object ext = {
  ext | 
    dim : Vector,
    pos : Vector,
    vel : Vector,
    gfx : Form
}