module Laser where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import Physics
import Object (Object)
import Vector (..)

type alias Laser = Object
  { dmg : Float
  }

update : Event -> Laser -> Laser
update event laser = laser

basicLaser =
  { dmg = 2
  , lifetime = 0
  , dim = vec 15 15
  , pos = vec 0 50
  , vel = vec 0 0
  , acc = vec 0 0 
  , gfx = F.rect 20 20 |> F.filled blue
  }