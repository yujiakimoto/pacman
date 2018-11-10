include Command

exception GhostNotFound

type color = RED | PINK | CYAN | ORANGE

type mode = CHASE | SCATTER | FRIGHTENED

type t = {
  position: int * int; 
  start_pos: int * int;
  direction : Command.direction; 
  target : int * int;
  speed: int; 
  mode: mode;
  color: color; 
}

let position g = 
  g.position

let direction g = 
  g.direction

let speed g = 
  g.speed

let color g = 
  g.color

let respawn g = 
  { g with position = g.start_pos; mode = SCATTER }

let frightened g = 
  g.mode = FRIGHTENED

let set_mode mode g = 
  { g with mode = mode }

let chase = 
  set_mode CHASE

let scatter = 
  set_mode SCATTER

let frighten = 
  set_mode FRIGHTENED

let blinky_pos ghosts = 
  let rec blinky_pos' = function 
    | [] -> raise GhostNotFound
    | g :: t -> if color g = RED then position g else blinky_pos' t
  in blinky_pos' ghosts

let target ghosts maze pac g = 
  let height, width = Maze.dimension maze in 
  let px, py = Pacman.position pac in 
  let dir = Pacman.direction pac in 
  match g.mode, g.color with 
  | CHASE, RED -> (px, py)
  | CHASE, PINK -> 
    let dx, dy = Command.dir_to_pos dir in (px+4*dx, py+4*dy)
  | CHASE, CYAN -> 
    let bx, by = blinky_pos ghosts in 
    let dx, dy = Command.dir_to_pos dir in (2*(px+2*dx)-bx, 2*(py+2*dy)-by)
  | CHASE, ORANGE -> 
    let gx, gy = position g in 
    if Util.distance (px, py) (gx, gy) <= 8.0 then (0, height+1) else (px, py)
  | SCATTER, RED -> (width-3, -2)
  | SCATTER, PINK -> (3, -2)
  | SCATTER, CYAN -> (width-1, height+1)
  | SCATTER, ORANGE -> (0, height+1)
  | FRIGHTENED, _ -> (0, 0)

let direction' maze target g =
  let gx, gy = g.position in 
  let dirs = Maze.valid_dirs ~ghost:true maze (gx, gy) in 
  let dirs = Util.remove dirs (Command.opposite g.direction) in 
  let dist dir = 
    let dx', dy' = Command.dir_to_pos dir in 
    Util.distance target (gx+dx', gy+dy')
  in let assoc = List.combine dirs (List.map dist dirs) in 
  (** Uses stable_sort to preserve direction priority if equidistant *)
  let sorted = List.stable_sort (fun x y -> compare (snd x) (snd y)) assoc in 
  if not (frightened g) then fst (List.hd sorted)
  else Util.random_sample dirs

let color_to_string = function 
  | RED -> "RED"
  | PINK -> "PINK"
  | CYAN -> "CYAN"
  | ORANGE -> "ORANGE"

let move ghosts maze pac g = 
  let pos = ref g.position in 
  let dir = ref g.direction in
  let tar = ref g.target in  
  (** If not at an intersection, keep moving in current direction;
      If at an intersection, update target & direction then move 
  *)
  if not (Maze.is_intersection ~ghost:true maze g.position) then 
    pos := Maze.next ~ghost:true !dir g.position maze
  else begin
    tar := target ghosts maze pac g;
    dir := direction' maze !tar g;
    pos := Maze.next ~ghost:true !dir g.position maze
  end;
  { g with position = !pos; direction = !dir; target = !tar }

let init n pos = 
  { 
    position = pos;
    start_pos = pos;
    direction = UP;
    target = 0, 0;
    speed = 1;
    mode = SCATTER;
    color = List.nth [RED; PINK; CYAN; ORANGE] n
  }

let init_list positions = 
  let ghosts = ref [] in 
  for i = 0 to 3 do 
    ghosts := init i (List.nth positions i) :: !ghosts
  done;
  !ghosts

let print_color g = 
  match color g with 
  | RED -> [ANSITerminal.red]
  | PINK -> [ANSITerminal.magenta]
  | CYAN -> [ANSITerminal.cyan]
  | ORANGE -> [ANSITerminal.green]