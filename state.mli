(** [t] is the type of state. *)
type t

(** [position] is the type of board position. *)
type position = int * int

(** [pacman state] is the pacman associated with [state]. *)
val pacman : t -> Pacman.t

(** [ghosts state] is the list of ghosts associated with [state]. *)
val ghosts : t -> Ghosts.t list

(** [score state] is the score of [state]. *)
val score : t -> int

(** [levels_completed state] is the levels completed of [state]. *)
val levels_completed : t -> int

(** [dead state] is true if and only if pacman should die given [state], 
    that is, pacman's position overlaps with the position of any ghost with
    evade mode off. *)
val dead : t -> bool

(** [init_state maze] is the state of new game of maze *)
val init_state : Maze.t -> t 

(** [game_over state] is true if and only if pacman has 0 lives left. *)
val game_over : t -> bool

(** [beat_level state] is true if and only if the level has been won, that is,
    all the pac-dots have been eaten. *)
val beat_level : t -> bool

(** [ate_ghost p p' g g'] is whether pacman was able to eat one of the ghosts 
    when changing state from [p] to [p'], with the ghosts changing state from
    [g] to [g']. *)
val ate_ghost : Pacman.t -> Pacman.t -> Ghosts.t list -> Ghosts.t list -> bool

(** [respawn maze state] is the starting state of [maze], with one less life
    than there was in [state]. *)
val respawn : Maze.t -> t -> t

(** [next_level maze state] is the next level given [maze] and [state]. *)
val next_level : Maze.t -> t -> t

(** [next maze state command] is the next state given the current state, maze, 
    and command. *)
val next : Maze.t -> t -> Command.cmd -> t

(** [add_pacman maze state] adds pacman to position in [maze]. *)
val add_pacman : (ANSITerminal.style list * string) array array -> t -> unit

(** [add_ghosts maze state] adds ghost to position in [maze]. *)
val add_ghosts : (ANSITerminal.style list * string) array array -> t -> unit

(** [add_ghosts maze state] adds pac dot to position in [maze]. *)
val add_pac_dots : (ANSITerminal.style list * string) array array -> t -> unit

(** [add_ghosts maze state] adds pellet to position in [maze]. *)
val add_power_pellets : 
  (ANSITerminal.style list * string) array array -> t -> unit

(** [add_ghosts maze state] adds fruit to position in [maze]. *)
val add_fruits : (ANSITerminal.style list * string) array array -> t -> unit


