(** [arr_to_list arr] converts an array of arrays to a list of lists. *)
val arr_to_list : 'a array array -> 'a list list

(** [list_to_arr] converts an lists of lists to a arrays of arrays. *)
val list_to_arr : 'a list list -> 'a array array

(** [position_to_string pos] is the string representation of the coordinates
    [pos]. *)
val position_to_string : int * int -> string

(** [remove lst pos] is [lst] with [pos] removed if [pos] is an element of 
    [lst], and [lst] otherwise. *)  
val remove : 'a list -> 'a -> 'a list

(** [modulo n d] is the positive remainder when [n] is divided by [d]. *)
val modulo : int -> int -> int

(** [decisecond ()] is the integer in the first decimal place of the current
    Unix system time. *)
val decisecond : unit -> int

(** [centisecond ()] is the integer in the second decimal place of the current
    Unix system time. *)
val centisecond : unit -> int

(** [even n] is true if and only if [n] is even. *)
val even : int -> bool

(** [distance (x1, y1) (x2, y2)] is the euclidian distance between the points
    [(x1, y1)] and [(x2, y2)]. *)
val distance : int * int -> int * int -> float

(** [random_sample lst] is a uniformly randomly sampled element from [lst]. *)
val random_sample : 'a list -> 'a