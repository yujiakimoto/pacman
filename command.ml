type direction = UP | DOWN | RIGHT | LEFT

type cmd = MOVE of direction | PAUSE | QUIT

let opposite = function 
  | UP    -> DOWN
  | DOWN  -> UP
  | LEFT  -> RIGHT
  | RIGHT -> LEFT

let dir_to_string = function
  | UP    -> "UP"
  | DOWN  -> "DOWN"
  | RIGHT -> "RIGHT"
  | LEFT  -> "LEFT"

let dir_to_pos = function 
  | UP    -> (0, -1)
  | DOWN  -> (0, 1)
  | LEFT  -> (-1, 0)
  | RIGHT -> (1, 0)