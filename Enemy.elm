module Enemy where


type alias Enemy =
  { x      : Float
  , y      : Float
  , vx     : Float
  , vy     : Float
  , status : EnemyStatus
  }

update_vel_enemy : Int -> Enemy -> Enemy
update_vel_enemy vx enemy =
  let
    {x, y, vx, vy, status} = enemy
  in if
    | x <= -areaW / 2 -> { enemy | vx <- -vx }
    | x >= areaW / 2 -> { enemy | vx <- -vx }
    | otherwise -> { enemy | vx <- vx }

update_pos_enemy : Time -> Enemy -> Enemy
update_pos_enemy dt enemy =
  let {x, y, vx, vy} = enemy in
  { enemy |
      x <- (x + dt * vx),
      y <- (y + dt * vy)
  }

update_status_enemy : Laser -> Enemy -> Enemy
update_status_enemy laser enemy =
  case (check_collision laser enemy) of
    Collision -> { enemy | status <- Dead }
    _         -> enemy

update_enemy : Enemy -> Game -> Time -> Enemy
update_enemy enemy game dt =
  let
    {state, player, laser, score, enemies} = game
    {x_laser, y_laser, status} = laser
    collision = check_collision laser enemy
  in enemy
    |> update_pos_enemy dt
    |> update_vel_enemy 20
    |> update_status_enemy laser


renderEnemy: Enemy -> C.Form
renderEnemy enemy =
  case enemy.status of
    Hostile ->
      C.rect 20 20
        |> C.filled blue
        |> C.move (enemy.x, enemy.y)
    Dead ->
      E.empty
        |> C.toForm
        |> C.move (enemy.x, enemy.y)