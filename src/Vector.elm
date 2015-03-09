module Vector where

type alias Vector = (Float, Float)

vec : Float -> Float -> Vector
vec x y = (x, y)

veci : Int -> Int -> Vector
veci x y = (toFloat x, toFloat y)

equal : Vector -> Vector -> Bool
equal (x1, y1) (x2, y2) =
  (x1 == x2) && (y1 == y2)

getX : Vector -> Float
getX (x, y) = x

getY : Vector -> Float
getY (x, y) = y

setX : Float -> Vector -> Vector
setX f (x, y) = (f, y)

setY : Float -> Vector -> Vector
setY f (x, y) = (x, f)

add : Vector -> Vector -> Vector
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub : Vector -> Vector -> Vector
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

cross : Vector -> Vector -> Vector
cross (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

dot : Vector -> Vector -> Float
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

bound : Vector -> Vector -> Vector -> Vector
bound (ax, ay) (bx, by) (x, y) = (clamp ax bx x, clamp ay by y)

scale : Float -> Vector -> Vector
scale f (x, y) = (f * x, f * y)