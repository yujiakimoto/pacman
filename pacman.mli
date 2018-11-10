(** [t] is the type of a pacman *)
type t

(** [lives pacman] is the remaining lives of [pacman] *)
val lives : t -> int

(** [position pacman] is the current position of [pacman] *)
val position : t -> int * int

(** [direction pacman] is the current direction of [pacman] *)
val direction : t -> Command.direction

(** [speed pacman] is the current speed of [pacman]*)
val speed : t -> int

(** TODO: Document *)
val move : Maze.t -> Command.direction -> t -> t

(** [init l (x, y), d)] initializes a pacman with [l] lives, starting
    at position [(x, y)] and moving in direction [d]. *)
val init : int -> int * int ->  Command.direction -> t
