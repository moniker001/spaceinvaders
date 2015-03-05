module Player where

import Color (..)
import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage as F
import Physics
import Object (Object, stepPosition)
import Vector (..)

type alias Player = Object
  { lives  : Float
  , hp     : Float
  , energy : Float
  }

update : Event -> Player -> Player
update ((delta, ks, { x, y }) as event) player =
  let pos' = stepPosition event player
  in
  { player | pos <- pos'
           }

initPlayer =
  { lives = 3
  , hp = 10
  , energy = 10
  , lifetime = 0
  , dim = vec 15 15
  , pos = startPos
  , vel = vec 100 100
  , acc = vec 0 0 
  , gfx = F.group [ F.rect 40 10 |> F.filled red
                  , F.oval 7 15  |> F.filled red |> F.moveY 5
                  ]
  }