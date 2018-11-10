include Command

type position = int * int

exception InvalidLocation of position
exception InvalidOperation
exception InvalidStart

(** Point values for eating pac-dots/fruit/ghosts *)
let pts_pac_dot = 10
let pts_fruit = 100
let pts_ghost = 200

type t = {
  game_timer: int;
  pacman: Pacman.t;
  pacman_start_pos: (int * int);
  invincible_timer: int option;
  ghosts: Ghosts.t list;
  ghosts_eaten: int;
  pac_dots: (int * int) list;
  power_pellets: (int * int) list;
  fruits: (int * int) option;
  score: int;
  levels_completed: int;
  dead: bool          
}

let pacman st = 
  st.pacman

let ghosts st = 
  st.ghosts

let score st = 
  st.score

let levels_completed st =
  st.levels_completed

let dead st = 
  st.dead

let init_state maze = 
  {
    game_timer = 0;
    pacman = Pacman.init 3 (Maze.center maze) UP;
    pacman_start_pos =  Maze.center maze;
    invincible_timer = None;
    ghosts = Ghosts.init_list (Maze.init_ghosts maze);
    ghosts_eaten = 0;
    pac_dots = Maze.init_empty maze;
    power_pellets = Maze.corners maze;
    fruits = None;
    score = -10;
    levels_completed = 0;
    dead = false;
  }

(** [update_fruit st] updates the fruit in [st] at the next game tick: either
    maintaing its current location or randomly generating. *)
let update_fruit st = 
  match st.fruits with 
  | None -> 
    if Random.int 100 <> 1 then None else Some st.pacman_start_pos
  | Some (x, y) -> 
    if Pacman.position st.pacman = (x, y) then None else Some (x, y)

(** [ate_fruit f f'] is true if and only if [f] is a fruit and [f'] is not. *)
let ate_fruit f f' = 
  match f with 
  | None -> 0
  | Some _ -> if f' = None then 1 else 0

(** [update_timer t] updates the timer [t] at the next game tick: either it 
    counts down by one, or remains expired. *)
let update_timer = function 
  | Some t -> if t > 0 then Some (t-1) else None
  | None -> None

(** [timer_expired t] is true if and only if [t] has one game tick remaining. *)
let timer_expired = function
  | Some 1 -> true
  | Some _ | None -> false

(** [timer_expiring st] is true if and only if [t] has less than or equal to 15
    game ticks remaining. *)
let timer_expiring st = 
  match st.invincible_timer with 
  | Some t -> if t <= 15 then true else false
  | None -> false

(** [same_position pac ghost] is whether pacman [pac] and ghost [ghost] occupy
    the same position on the maze. *)
let same_position pac ghost = 
  if Pacman.position pac = Ghosts.position ghost then true else false

(** [switched_position p p' g g'] is whether pacman and a ghost switched 
    positions on the maze when pacman goes from [p] to [p'] and the ghost
    goes from [g] to [g'] *)
let switched_position pac pac' ghost ghost' = 
  let pos = Pacman.position pac in 
  let pos' = Pacman.position pac' in 
  if Ghosts.position ghost = pos' && Ghosts.position ghost' = pos
  then true else false

(** [update_ghosts_lives p p' g g'] is the updated list of ghosts when pacman
    goes from [p] to [p'] and the ghosts fo from [g] to [g']. Any ghost that 
    collided with pacman will respawn in the center of the maze. *)
let update_ghosts_lives pac pac' ghosts ghosts' = 
  let rec update_ghosts' = function 
    | [] -> []
    | (g, g') :: t -> 
      if not (same_position pac' g' || switched_position pac pac' g g') then 
        g' :: update_ghosts' t 
      else 
        (if not (Ghosts.frightened g') then g' :: update_ghosts' t 
         else (Ghosts.respawn g') :: update_ghosts' t)
  in update_ghosts' (List.combine ghosts ghosts')

(** [level_to_intervals lvl] is the list of game-ticks at which ghosts
    alternate between scatter and chase mode during level [lvl]. *)
let level_to_intervals lvl = 
  let t = match lvl with 
    | 1 ->         [7; 27; 34; 54; 59; 79;   84]
    | 2 | 3 | 4 -> [7; 27; 34; 54; 59; 1092; 1093] 
    | _ ->         [5; 25; 30; 50; 55; 1092; 1093]
  in List.map (fun x -> 10 * x) t

(** [timer_to_mode lvl time] is the interval number after [time] game-ticks
    in level [lvl]. *)
let timer_to_mode lvl time = 
  let intervals = level_to_intervals lvl in 
  let rec f = function 
    | [] -> List.length intervals
    | h :: t ->
      if time <= h then List.length intervals - List.length t - 1 else f t
  in f intervals

(** [update_ghosts_mode lvl time ghosts] is the updated list of ghosts to the
    new mode after [time] game-ticks in level [lvl]. *)
let update_ghosts_mode lvl time ghosts = 
  let n = timer_to_mode lvl time in 
  List.map (if Util.even n then Ghosts.scatter else Ghosts.chase) ghosts 

let ate_ghost pac pac' ghosts ghosts' = 
  let ghost_list = List.combine ghosts ghosts' in 
  let rec ate_ghost' = function 
    | [] -> false
    | (g, g') :: t -> 
      same_position pac' g' || switched_position pac pac' g g' || ate_ghost' t
  in ate_ghost' ghost_list

let respawn maze st = 
  let lives = st.pacman |> Pacman.lives in 
  {
    st with 
    game_timer = 0;
    pacman = Pacman.init (lives-1) (Maze.center maze) UP;
    ghosts = List.map Ghosts.respawn st.ghosts
  }

let next_level maze st = 
  let lives = st.pacman |> Pacman.lives in 
  {
    game_timer = 0;
    pacman = Pacman.init lives (Maze.center maze) UP;
    pacman_start_pos = st.pacman_start_pos;
    invincible_timer = None;
    ghosts = Ghosts.init_list (Maze.init_ghosts maze);
    ghosts_eaten = 0;
    pac_dots = Maze.init_empty maze;
    power_pellets = Maze.corners maze;
    fruits = None;
    score = st.score;
    levels_completed = st.levels_completed + 1;
    dead = false;
  }

let is_dead pac pac' ghosts ghosts' = 
  let rec is_dead' = function 
    | [] -> false
    | (g, g') :: t -> 
      (((same_position pac' g' || switched_position pac pac' g g') 
        && (not (Ghosts.frightened g')) || is_dead' t))
  in is_dead' (List.combine ghosts ghosts')

let beat_level st = 
  List.length st.pac_dots = 0

let game_over st = 
  (st.pacman |> Pacman.lives) <= 0

let next maze st = function
  | MOVE dir -> 
    let game_timer' = ref st.game_timer in  
    let pacman' = ref st.pacman in 
    let ghosts' = ref st.ghosts in 
    let invinc_timer' = ref st.invincible_timer in
    let ghosts_eaten' = ref st.ghosts_eaten in 
    let pac_dots' = ref st.pac_dots in 
    let power_pellets' = ref st.power_pellets in 
    let fruits' = ref st.fruits  in
    let score' = ref st.score in 

    pacman' := Pacman.move maze dir st.pacman;
    ghosts' := List.map (Ghosts.move st.ghosts maze st.pacman) !ghosts';
    invinc_timer' := update_timer !invinc_timer';
    pac_dots' := Util.remove !pac_dots' (Pacman.position !pacman');
    power_pellets' := Util.remove !power_pellets' (Pacman.position !pacman');
    fruits' := update_fruit st;

    let dead' = is_dead st.pacman !pacman' st.ghosts !ghosts' in
    let killed_ghost = ate_ghost st.pacman !pacman' st.ghosts !ghosts' in 
    if killed_ghost then (Unix.sleep 1; ghosts_eaten' := !ghosts_eaten' + 1);
    ghosts' := update_ghosts_lives st.pacman !pacman' st.ghosts !ghosts';
    score' := !score' 
              + pts_pac_dot * List.compare_lengths st.pac_dots !pac_dots'
              + pts_fruit * ate_fruit st.fruits !fruits'
              + pts_ghost * !ghosts_eaten' * if killed_ghost then 1 else 0;

    (** Make ghosts chase/scatter based on game timer *)
    game_timer' := !game_timer' + if !invinc_timer' = None then 1 else 0;
    let level = st.levels_completed + 1 in 
    if List.mem st.game_timer (level_to_intervals level)
    then ghosts' := update_ghosts_mode level !game_timer' !ghosts';

    (** When invincible, make ghosts edible *)
    let invincible = List.compare_lengths st.power_pellets !power_pellets' = 1 
    in if invincible then (ghosts' := List.map Ghosts.frighten !ghosts'; 
                           invinc_timer' := Some 70);
    (** When timer expires, restore ghosts to normal state *)
    if timer_expired st.invincible_timer 
    then (ghosts' := List.map Ghosts.scatter !ghosts'; ghosts_eaten' := 0);

    { 
      game_timer = !game_timer';
      pacman = !pacman';
      pacman_start_pos = st.pacman_start_pos;
      invincible_timer = !invinc_timer';
      ghosts = !ghosts';
      ghosts_eaten = !ghosts_eaten';
      pac_dots = !pac_dots';
      power_pellets = !power_pellets';
      fruits = !fruits';
      score = !score';
      levels_completed = st.levels_completed;
      dead = dead'
    } 

  | PAUSE -> raise InvalidOperation
  | QUIT -> raise InvalidOperation

let add_pacman maze st = 
  let (x, y) = Pacman.position st.pacman in 
  let str = 
    match Pacman.direction st.pacman with 
    | UP    -> if Util.(decisecond () |> even) then "V" else "U"
    | DOWN  -> if Util.(decisecond () |> even) then "^" else "∩"
    | LEFT  -> if Util.(decisecond () |> even) then ">" else ")"
    | RIGHT -> if Util.(decisecond () |> even) then "<" else "("
  in maze.(y).(x) <- ([ANSITerminal.yellow; ANSITerminal.Bold], str)

(** [add_ghost maze g flash] adds a ghost [g] to [maze]. If [flash] is true,
    the ASCII representation will be made to blink when displayed. *)
let add_ghost maze g flash = 
  let (x, y) = Ghosts.position g in 
  let printer = 
    if Ghosts.frightened g then (
      if flash && Util.(decisecond () |> even) 
      then (Ghosts.print_color g, "ᗣ") else ([ANSITerminal.on_blue], "ᗣ")) 
    else (Ghosts.print_color g, "ᗣ") in
  maze.(y).(x) <- printer

let add_ghosts maze st =
  let rec add_ghosts' = function 
    | [] -> ()
    | h :: t -> add_ghost maze h (timer_expiring st); add_ghosts' t
  in add_ghosts' st.ghosts

let add_pac_dots maze st =
  let rec add_pac_dots' = function 
    | [] -> ()
    | (x, y) :: t 
      -> maze.(y).(x) <- ([ANSITerminal.white], "·"); add_pac_dots' t
  in add_pac_dots' st.pac_dots 

let add_power_pellets maze st =
  let rec add_pellets' = function 
    | [] -> ()
    | (x, y) :: t -> 
      maze.(y).(x) <- ([ANSITerminal.white; ANSITerminal.Bold], "•"); 
      add_pellets' t
  in add_pellets' st.power_pellets

let add_fruits maze st =
  let rec add_fruits' maze = function
    | Some (x, y) ->  maze.(y).(x) <- ([ANSITerminal.green], "ó")
    | None -> ()
  in add_fruits' maze st.fruits
