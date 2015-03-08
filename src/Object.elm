module Object where

import Event (Event)
import Event (..)
import Global (..)
import Graphics.Collage (Form)
import Graphics.Collage as Form
import List (filter)
import Physics
import Vector (Vector, vec)
import Vector as V

type ObjectType = Player
                | Laser
                | Enemy

type alias Object ext =
  { ext
  | lifetime : Float
  , objtype  : ObjectType
  , dim      : Vector
  , pos      : Vector
  , vel      : Vector
  , acc      : Vector
  , gfx      : Form
  , rem      : Bool
  }

render : Object a -> Form
render object = Form.move object.pos object.gfx

type CollisionType = EnemyCollision
                   | LaserCollision
                   | PlayerCollision
                   | NoCollision

checkCollision : Object a -> List (Object b) -> CollisionType
checkCollision src objects = case objects of
  [] -> NoCollision
  h::t -> if | Physics.isColliding src.pos src.dim h.pos h.dim ->
                 case h.objtype of
                   Enemy  -> EnemyCollision
                   Player -> PlayerCollision
                   Laser  -> LaserCollision
             | otherwise -> checkCollision src t

garbageCollect : List (Object a) -> List (Object a)
garbageCollect objects =
  filter (\o -> o.rem == False) objects