module Laser where

type alias Laser =
  { x_laser : Float
  , y_laser : Float
  , vx      : Float
  , vy      : Float
  , status  : LaserStatus
  , fired   : Time
  }

update_pos_laser : Time -> Player -> Laser -> Laser
update_pos_laser dt player laser =
  let
    { x_laser, y_laser, status, fired } = laser
    { x, y } = player
  in
  case status of
    Shooting -> if
      | y_laser > 250 ->
          { laser |
              x_laser <- x,
              y_laser <- -215,
              status  <- Ready
          }
      | otherwise -> 
          { laser | y_laser <- y_laser + dt * 5 }
    Ready ->
      { laser | 
          x_laser <- x,
          y_laser <- -215 
      }
    Collision -> { laser | y_laser <- -215 }

update_status_laser : Bool -> Enemy -> Laser -> Laser
update_status_laser shoot enemy laser =
  let {x_laser, y_laser, vx, vy, status, fired} = laser in
  case status of
    Ready -> if
      | shoot -> { laser | status <- Shooting}
      | otherwise -> laser
    Shooting ->  { laser | status <- check_collision laser enemy }     
    Collision -> { laser | status <- Ready}


renderLaser : Laser -> C.Form
renderLaser laser =
  case laser.status of
    Ready -> 
      C.rect 1 15
        |> C.filled black
        |> C.move (laser.x_laser, laser.y_laser)
    _ ->
      C.rect 1 15
        |> C.filled white
        |> C.move (laser.x_laser, laser.y_laser)
