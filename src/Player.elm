module Player where

import Color (..)
import Event (Event)
import Event (..)
import Graphics.Collage as F
import Physics
import Object (Object)
import Vector (..)

type alias Player = Object
  { hp : Float
  , energy : Float
  }

update : Event -> Player -> Player
update event player = player

initPlayer =
  { hp = 10
  , energy = 10
  , lifetime = 0
  , dim = vec 15 15
  , pos = vec 0 -220
  , vel = vec 0 0
  , acc = vec 0 0 
  , gfx = F.group [ F.rect 40 10 |> F.filled red
                  , F.oval 7 15 |> F.filled red |> F.moveY 5
                  ]
  }