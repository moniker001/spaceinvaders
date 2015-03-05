module Enemy where

import Event (Event)
import Event (..)
import Physics
import Object (Object)

type alias Enemy = Object
  { hp : Float
  }

update : Event -> Enemy-> Enemy
update event enemy = enemy

basicEnemy =
  { hp = 10
  , lifetime = 0
  , dim = vec 15 15
  , pos : vec 0 50
  , vel : vec 0 0
  , acc : vec 0 0 
  , gfx : F.rect 20 20 |> F.filled blue
  }