(** Abstract type representing directions a character can move. *)
type direction = UP | DOWN | RIGHT | LEFT

(** Abstract type representing a command that a user can issue. *)
type cmd = MOVE of direction | PAUSE | QUIT

(** [opposite dir] is the opposite direction of [dir]. *)
val opposite : direction -> direction

(** [dir_to_string dir] is the string representation of a direction. *)
val dir_to_string : direction -> string

(** [dir_to_pos dir] is the change in coordinates when moving in direction
    [dir]. *)
val dir_to_pos : direction -> int * int