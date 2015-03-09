module Event where

import Keyboard (KeyCode)
import Keyboard

type alias Event = (Float, Input)

type Input = Tap Int
           | Pressed (List Int)
           | Tick

getDelta : Event -> Float
getDelta (delta, _) = delta