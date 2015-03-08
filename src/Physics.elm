module Physics where

import Vector (Vector, vec)
import Vector

isColliding : Vector -> Vector -> Vector -> Vector -> Bool
isColliding (xA, yA) (wA, hA) (xB, yB) (wB, hB) = 
  let (minXA, minYA) = (xA - (wA / 2), yA - (hA / 2))
      (minXB, minYB) = (xB - (wB / 2), yB - (hB / 2))
  in if
  | (minXA < minXB + wB) && 
    (minXA + wA > minXB) && 
    (hA < minYB + hB) && 
    (hA + minYA > minYB) ->
      True
  | otherwise -> False
{--
  let (minXA, minYA) = (xA - (wA / 2), yA - (hA / 2))
      (minXB, minYB) = (xB - (wB / 2), yB - (hB / 2))
      (maxXA, maxYA) = (xA + (wA / 2), yA + (hA / 2))
      (maxXB, maxYB) = (xB + (wB / 2), yB + (hB / 2))
  in
      if | (minXA - maxXB) > 0 -> False
         | (minYA - maxYB) > 0 -> False
         | (minXB - maxXA) > 0 -> False
         | (minYB - maxXB) > 0 -> False
         | otherwise           -> True
--}