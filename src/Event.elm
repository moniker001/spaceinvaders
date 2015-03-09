module Event where

import Keyboard (KeyCode)
import Keyboard

type alias Event = (Float, List KeyCode, { x : Int, y : Int })
