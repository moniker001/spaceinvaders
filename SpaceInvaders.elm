module SpaceInvaders where

import List ((::))
import List
import Color (..)
import Signal ((<~),(~),Signal)
import Signal
import Window
import Mouse
import Keyboard
import Text (plainText)
import Text as T
import Graphics.Element (Element)
import Graphics.Element as E
import Graphics.Collage as C
import Graphics.Input (button, customButton)
import Time (Time)
import Time


areaW = 500
areaH = 500

type State = MoveL | MoveR | Idle

type alias Object =
  { x:Float, y:Float, vx:Float, dir:State}

player : Object
player =
  { x=0, y=-220, vx=0, dir=Idle}

-- UPDATE

update : (Time, { x:Int, y:Int }) -> Object -> Object
update (timeDelta, direction) obj =
  obj
    |> setVelocity direction
    |> setDirection direction
    |> setPosition timeDelta

setVelocity : { x:Int, y:Int } -> Object -> Object
setVelocity {x,y} obj =
  let
    newVel n =
      if x == 0 || y == 0
        then toFloat n
        else toFloat n / sqrt 2
  in
      { obj |
          vx <- newVel x
      }

setDirection : { x:Int, y:Int } -> Object -> Object
setDirection {x,y} obj =
  { obj |
      dir <-
        if  | x > 0 -> MoveR
            | x < 0 -> MoveL
            | otherwise -> obj.dir
  }


setPosition : Time -> Object -> Object
setPosition dt ({x,y,vx,dir} as obj) =
  { obj |
      x <- clamp (-areaW/2) (areaW/2) (x + dt * vx)
  }


-- VIEW

view : (Int,Int) -> Object -> Element
view (w,h) {x,y,vx,dir} =
  let
    w' = toFloat (w - 1)
    h' = toFloat (h - 1)
  in
  E.container w h E.middle
    <| C.collage areaW areaH
        [ C.rect w' h'
          |> C.filled black
        , [ C.rect 40 10
            |> C.filled red,
            C.oval 7 15
            |> C.filled red
            |> C.moveY 5
          ]
            |> C.group
            |> C.move (x,y)
        , "Space Invaders"
          |> T.fromString
          |> T.color white
          |> T.height 40
          |> T.centered
          |> C.toForm
          |> C.moveY 220
        ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp update player input)

input : Signal (Time, { x:Int, y:Int })
input =
  Signal.sampleOn delta
    (Signal.map2 (,) delta Keyboard.wasd)

delta : Signal Time
delta =
  Signal.map (\t -> t / 20) (Time.fps 30)
