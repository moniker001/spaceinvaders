module Physics where

import Vector (Vector, vec)
import Vector

isColliding : Vector -> Vector -> Vector -> Vector -> Bool
isColliding (xA, yA) (wA, hA) (xB, yB) (wB, hB) = 
  let (minXA, minYA) = (xA - (wA / 2), yA - (hA / 2))
      (minXB, minYB) = (xB - (wB / 2), yB - (hB / 2))
      (maxXA, maxYA) = (xA + (wA / 2), yA + (hA / 2))
      (maxXB, maxYB) = (xB + (wB / 2), yB + (hB / 2))
  in if
  | (minXA < minXB + wB) && 
    (minXA + wA > minXB) && 
    (minYA < minYB + hB) && 
    (hA + minYA > minYB) ->
      True
  | otherwise -> False