type t = {
  lives: int;
  position: int * int; 
  direction: Command.direction; 
  speed: int;
}

let lives p = 
  p.lives

let position p = 
  p.position

let direction p = 
  p.direction

let speed p = 
  p.speed

let change_speed p s = 
  { p with speed = s }

let move maze dir p = 
  let valid = Maze.valid_dirs maze p.position in 
  let dir' = if List.mem dir valid then dir else p.direction in 
  let pos' = Maze.next dir' p.position maze in 
  { p with position = pos'; direction = dir' }

let init lives (x,y) dir = {
  lives = lives;
  position = (x,y);
  direction = dir;
  speed = 1;
}