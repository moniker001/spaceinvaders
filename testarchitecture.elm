module SpaceInvaders where

import Math.Vector2 (Vec2, vec2)
import Math.Vector2 as Vec2
import Time (..)
import Signal (Signal, (<~), (~))
import Signal
import Graphics.Collage as C
import Graphics.Element (Element)
import Graphics.Element as E
import List ((::))
import Color (..)
import Window
import Keyboard
import Text (asText)

-- Globals

gwidth  = 800
gheight = 600

-- State

type alias State = impor
  { runtime   : Float
  , gamestate : GameState
  , objects   : List Object
  }

type GameState = Playing | Paused

upState : Event -> State -> State
upState event state = case event of
  (delta, _) ->
  { state | runtime   <- state.runtime + delta
          , gamestate <- state.gamestate
          , objects   <- upObjects event state.objects
          }

rndrState : State -> List C.Form
rndrState state =
  let box     = C.outlined (C.solid black) (C.rect gwidth gheight)
      objects = rndrObjects state.objects
  in
  box::objects

initState : State
initState =
  { runtime = 0
  , gamestate = Paused
  , objects = [player]
  }

currentState : Signal State
currentState = Signal.foldp upState initState sEvent

-- Event

gdelta : Signal Time
gdelta = inSeconds <~ fps 30

type Action m = None | Move m
type alias Event = (Float, Action { x : Int, y : Int} )

sEvent : Signal (Float, Action { x : Int, y : Int })
sEvent = ((\t a -> (t, Move a)) <~ gdelta ~ (Keyboard.arrows))

dToV : { x : Int, y : Int } -> Vec2
dToV { x, y } = vec2 (toFloat x) (toFloat y)

-- Object

type alias Object =
  { pos : Vec2
  , vel : Vec2
  , acc : Vec2
  , res : Vec2
  , gfx : C.Form
  }

upObject : Event -> Object -> Object
upObject event obj = case event of
  (delta, None) ->
  { obj | pos <- Vec2.add obj.pos (vec2 0 0)--(Vec2.scale delta obj.vel)
        , vel <- obj.vel--Vec2.add obj.vel (Vec2.scale delta obj.acc)
        , acc <- Vec2.add obj.acc (Vec2.scale delta obj.res) }
  (delta, Move { x, y }) ->
  let x' = Vec2.getX obj.vel
      y' = Vec2.getY obj.vel
      x_ = toFloat x
      y_ = toFloat y
      nVel = vec2 (x' * x_) (y' * y_)
  in
  { obj | pos <- Vec2.add obj.pos (Vec2.scale delta nVel)
        , vel <- obj.vel--Vec2.add obj.vel (Vec2.scale delta obj.acc)
        , acc <- Vec2.add obj.acc (Vec2.scale delta obj.res) }
 
upObjects : Event -> List Object -> List Object
upObjects event objs = case objs of
  []   -> []
  h::t -> (upObject event h) :: (upObjects event t)

rndrObject : Object -> C.Form
rndrObject obj =
  C.move ((Vec2.getX obj.pos), (Vec2.getY obj.pos)) (obj.gfx)

rndrObjects : List Object -> List C.Form
rndrObjects objs = case objs of
  [] -> []
  h::t -> (rndrObject h) :: (rndrObjects t)

player : Object
player =
  { pos = vec2 0 -200
  , vel = vec2 100 100
  , acc = vec2 0 0
  , res = vec2 0 0
  , gfx = C.rect 40 10 |> C.filled red
  }

laser : Object
laser =
  { pos = vec2 0 -200
  , vel = vec2 100 100
  , acc = vec2 0 0
  , res = vec2 0 0
  , gfx = C.rect 5 20 |> C.filled red
  }


-- VIEW

view : (Int, Int) -> State -> Element
view (w, h) state =
  C.collage gwidth gheight (rndrState state)

main : Signal Element
--main = view <~ Window.dimensions ~ currentState
main = Signal.map asText sEvent