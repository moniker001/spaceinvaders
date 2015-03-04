module Object where

import Vector (..)
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

update_pos : Time -> Object a -> Object a
update_pos dt object =
  let
    x = (fst object.dim) / 2
    y = (snd object.dim) / 2
  in
  { object |
      pos <- vclamp (-areaW/2+x,-areaH/2+y) (areaW/2-x,areaH/2-y) (vadd object.pos (vscale dt object.vel))
  }