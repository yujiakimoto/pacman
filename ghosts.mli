(** Type representing the color of a ghost. *)
type color = RED | PINK | CYAN | ORANGE

(** The movement mode of the ghost. This dictates the ghost's decisions on
    which direction to move in at each game-tick. *)
type mode = CHASE | SCATTER | FRIGHTENED

(** Type representing a ghost. *)
type t

(** [position g] is the current position of ghost [g]. *)
val position : t -> int * int

(** [direction g] is the current direction in which ghost [g] is moving. *)
val direction : t -> Command.direction

(** [speed g] is the current speed of ghost [g]. *)
val speed : t -> int

(** [color g] is the color of ghost [g]. *)
val color : t -> color

(** [respawn g] is ghost [g] respawned, i.e. regenerated in the center of the
    maze with evade mode off. *)
val respawn : t -> t 

(** [evade g] is whether or not ghost [g] is in evade mode. *)
val frightened : t -> bool

(** [chase g] is [g] set to chase mode. *)
val chase : t -> t

(** [scatter g] is [g] set to scatter mode. *)
val scatter : t -> t

(** [frighten g] is [g] set to frightened mode. *)
val frighten : t -> t

(** [move ghosts maze pac g] is ghost [g] moved to its next position based
    on the positions of pacman [pac], the other ghosts [ghosts], and [maze]. *)
val move : t list -> Maze.t -> Pacman.t -> t -> t

(** [init_list] is the list of ghosts in a new game. *)
val init_list : (int * int) list -> t list 

(** [print_color g] is the style in which ghost [g] is to be printed. *)
val print_color : t -> ANSITerminal.style list