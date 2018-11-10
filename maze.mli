(** 
    Representation of static information of the game. 

    This module represents the data stored in game files, namely
    the location of the walls defining the maze in which the game
    is played. It handles loading of that data from a .csv file as 
    well as querying the data.
*)

(** Raised when attempting to load a file that does not describe
    a pacman game, because an invalid character was encountered.
*)

exception InvalidPosition of string

exception InvalidCharacter of string
(** Raised when attempting to load a file that does not describe
    a pacman game, because the maze is not rectangular.
*)
exception MalformedFile of string

(** The abstract type of values representing elements of a maze. *)
type element = WALL | EMPTY | VOID | CAGE | DOOR

(** The abstract type of values representing a maze. *)
type t = element array array

(** [rep_ok maze] confirms valid maze csv is entered *)
val rep_ok: t -> t

(** [dimension] returns the dimensions (height, width) of the maze. *)
val dimension : t -> int * int

(** [load file] loads a maze from the .csv file specified by [file]. *)
val load : string -> t

(** [element_at] returns the position of the elements added to the maze *)
val element_at : t -> int * int -> element

(** [to_printer m] is the representation of maze [m] required to redner it to
    the terminal. Each element is a tuple of the ANSITerminal styles in which
    to render the element at that position, and the string to print. *)
val to_printer : t -> (ANSITerminal.style list * string) array array

(** [valid_dirs ghost maze (x, y)] is the list of directions that pacman 
    (or ghost if [ghost]) can move in [maze] when at position (x, y). *)
val valid_dirs : ?ghost:bool -> t -> int * int -> Command.direction list

(** [is_intersection ghost maze (x, y)] is whether position (x, y) is an 
    intersection (i.e. the character can move vertically or horizontally) 
    in [maze] for pacman (or ghost if [ghost]). *)
val is_intersection : ?ghost:bool -> t -> int * int -> bool

(** [center maze] are the coordinates of the closest element to the center
    that is unoccupied *)
val center : t -> int * int

(** [corners maze] are the coordinates representing the 4 corners of the maze. 
*)
val corners : t -> (int * int) list

(** [next ghost dir (x, y) maze] is the next position in [maze] starting at
    position (x, y) and moving in direction [dir] for pacman (or ghost if 
    [ghost]). *)
val next : ?ghost:bool -> Command.direction -> int * int -> t -> int * int

(** [find_in elt maze] is the list of positions at which the element at [maze]
    is equal to [elt]. *)
val find_in : element -> t -> (int * int) list

(** [init_empty maze] is the coordinates of all positions that are initally 
    empty. *)
val init_empty : t -> (int * int) list 

(** [init_ghosts maze] is the coordinates of all positions that are initially
    occupied by a ghost. *)
val init_ghosts : t -> (int * int) list 
