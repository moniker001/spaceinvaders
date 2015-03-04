module Player where

type Move = MoveL | MoveR | Idle

type alias Player =
  { x    : Float
  , y    : Float
  , vx   : Float
  , vy   : Float
  , move : Move
  }

update_vel_player : Int -> Player -> Player
update_vel_player x player =
  let
    newVel n =
      if x == 0
        then toFloat n
        else toFloat n * 2
  in
      { player |
          vx <- newVel x
      }

update_move_player : Int -> Player -> Player
update_move_player x player =
  { player |
      move <-
        if  | x > 0 -> MoveR
            | x < 0 -> MoveL
            | otherwise -> player.move
  }

update_pos_player : Time -> Player -> Player
update_pos_player dt player =
  let {x, y, vx, vy, move} = player in
  { player |
      x <- clamp (-areaW/2) (areaW/2) (x + dt * vx)
  }



renderPlayer : Player -> C.Form
renderPlayer player =
  [ C.rect 40 10
    |> C.filled red
  , C.oval 7 15
    |> C.filled red
    |> C.moveY 5
  ]
    |> C.group
    |> C.move (player.x, player.y)
