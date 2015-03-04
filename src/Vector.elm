module Vector where

type alias Vector = (Float, Float)

vector : Float -> Float -> Vector
vector x y = (x, y)

vadd : Vector -> Vector -> Vector
vadd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vsub : Vector -> Vector -> Vector
vsub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vcross : Vector -> Vector -> Vector
vcross (x1, y1) (x2, y2) = (x1 * x2, y1 * y2)

vdot : Vector -> Vector -> Float
vdot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

vclamp : Vector -> Vector -> Vector -> Vector
vclamp (ax, ay) (bx, by) (x, y) = (clamp ax bx x, clamp ay by y)

vscale : Float -> Vector -> Vector
vscale f (x, y) = (f * x, f * y)