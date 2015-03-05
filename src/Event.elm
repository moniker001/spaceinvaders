module Event where

import Keyboard (KeyCode)
import Keyboard

type alias Event = (Float, List KeyCode, { x : Int, y : Int })

getDelta : Event -> Float
getDelta (delta, keysDown, arrows) = delta

