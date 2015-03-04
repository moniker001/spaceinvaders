check_collision : (Float,Float) -> (Float,Float) -> (Float,Float) -> (Float,Float) -> Bool
check_collision (x1,y1) (w1,h1) (x2,y2) (w2,h2) =
  let
  -- bottom left
    (min_x1, min_y1) = (x1 - (w1 / 2), y1 - (h1 / 2))
    (min_x2, min_y2) = (x2 - (w2 / 2), y2 - (h2 / 2))
  -- upper right
    (max_x1, max_y1) = (x1 + (w1 / 2), y1 + (h1 / 2))
    (max_x2, max_y2) = (x2 + (w2 / 2), y2 + (h2 / 2))
  in
  if
    | (min_x1 - max_x2) > 0 -> False
    | (min_y1 - max_y2) > 0 -> False
    | (min_x2 - max_x1) > 0 -> False
    | (min_y2 - max_y1) > 0 -> False
    | otherwise             -> True