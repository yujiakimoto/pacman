include Command

exception InvalidCharacter of string
exception MalformedFile of string
exception InvalidPosition of string

type element = WALL | EMPTY | VOID | CAGE | DOOR

type t = element array array

let dimension maze = 
  Array.length maze, Array.length maze.(0)

let element_at maze pos = 
  let x, y = pos in maze.(y).(x)

(** [str_to_element s] is the element represented by [s]. *)
let str_to_element = function 
  | "#" -> WALL
  | ""  -> EMPTY
  | "-" -> VOID
  | "*" -> CAGE
  | "D" -> DOOR
  | s -> raise (InvalidCharacter s)

(** [element_to_str e] is the string representation of [e] to be displayed on
    the terminal. *)
let element_to_printer = function
  | WALL  -> ([ANSITerminal.blue], "█")
  | EMPTY -> ([ANSITerminal.blue], " ")
  | VOID  -> ([ANSITerminal.black], " ")
  | CAGE  -> ([ANSITerminal.red], " ")
  | DOOR  -> ([ANSITerminal.yellow], "-")


let to_printer maze = 
  Array.map (Array.map element_to_printer) maze 

let element_to_str = function
  | WALL  -> "WALL"
  | EMPTY -> "EMPTY"
  | VOID  -> "VOID"
  | CAGE  -> "CAGE"
  | DOOR  -> "DOOR"

let valid_dirs ?(ghost: bool=false) maze pos = 
  let height, width = dimension maze in
  let x, y = pos in 
  let valid elts = 
    let dirs = ref [] in
    (** Ordered in reverse order of priority *)
    if List.mem (element_at maze (Util.modulo (x + 1) width, y)) elts then
      dirs := RIGHT :: !dirs;
    if List.mem (element_at maze (x, Util.modulo (y + 1) height)) elts then 
      dirs := DOWN :: !dirs;
    if List.mem (element_at maze (Util.modulo (x - 1) width , y)) elts then 
      dirs := LEFT :: !dirs;
    if List.mem (element_at maze (x, Util.modulo (y - 1) height)) elts then 
      dirs := UP :: !dirs;
    if List.length !dirs = 0 then raise (InvalidCharacter "") else !dirs
  in match element_at maze pos with 
  | WALL | VOID -> raise (InvalidPosition "On Wall/Void")
  | DOOR -> if not ghost then raise 
        (InvalidPosition "Pacman on door") else valid [EMPTY]
  | CAGE -> if not ghost then raise 
        (InvalidPosition "Pacman in cage") else valid [CAGE; DOOR]
  | EMPTY -> valid [EMPTY]

let is_intersection ?(ghost: bool=false) maze pos = 
  let x, y = pos in 
  if not (ghost && element_at maze (x, y+1) = DOOR) 
  then begin 
    let dirs = valid_dirs ~ghost:ghost maze pos in 
    (List.mem UP dirs || List.mem DOWN dirs) && 
    (List.mem LEFT dirs || List.mem RIGHT dirs)
  end
  else true 

let center maze = 
  let height, width = dimension maze in
  let x, y = (width/2, height/2) in
  let rec center' maze x y = 
    match element_at maze (x, y) with
    | WALL | VOID | CAGE | DOOR  -> center' maze x (y+1)
    | EMPTY -> (x, y)
  in center' maze x y

let corners maze = 
  let height, width = dimension maze in 
  [(1, 1); (1, height-2); (width-2, 1); (width-2, height-2)]

let next ?(ghost: bool=false) dir (x, y) maze = 
  (** TODO: Add timer for when ghosts can initially escape? *)
  let dir = if ghost && List.mem (element_at maze (x, y)) [CAGE; DOOR] 
    then UP else dir in 
  let height, width = dimension maze in 
  let dx, dy = Command.dir_to_pos dir in 
  let x', y' = Util.modulo (x+dx) width, Util.modulo (y+dy) height in 
  match element_at maze (x', y') with 
  | WALL | VOID -> (x, y) 
  | CAGE | DOOR -> if ghost && element_at maze (x, y) = CAGE then (x', y') 
    else (x, y)
  | EMPTY -> (x', y')


let find_in elt maze = 
  let height, width = dimension maze in 
  let positions = ref [] in 
  for y = 0 to height - 1 do 
    for x = 0 to width - 1 do 
      if maze.(y).(x) = elt then positions := (x, y) :: !positions 
    done
  done;
  !positions

let init_empty = 
  find_in EMPTY

let init_ghosts =   
  find_in CAGE

(** [rep_ok maze] is [maze] iff [maze] satisfies the representation invariant.
    Raises: MalformedFile if invariant is violated. *)
let rep_ok maze =
  let check_ghosts maze = 
    if List.length (find_in CAGE maze) = 4 then maze else 
      raise (MalformedFile "There must be exactly 4 ghosts") in 

  let check_corners maze = 
    let rec check_corners' = function 
      | [] -> true
      | h :: t -> 
        if element_at maze h = EMPTY then check_corners' t else false in 
    let ok = check_corners' (corners maze) in 
    if ok then maze else 
      raise (MalformedFile "Need spot for power pellets") in

  let check_wraparounds maze = 
    let height, width = dimension maze in 
    let pos = ref [] in 
    let ok = ref true in     
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do 
        if (x = 0 || x = width - 1) && (y = 0 || y = height - 1) && 
           element_at maze (x, y) = EMPTY then ok := false
        else if (x = 0 || x = width - 1 || y = 0 || y = height - 1) 
             && element_at maze (x, y) = EMPTY then pos := (x, y) :: !pos
      done
    done;
    for i = 0 to List.length !pos - 1 do 
      let x, y = List.nth !pos i in 
      let x' = if x = 0 then width-1 else if x = width-1 then 0 else x in 
      let y' = if y = 0 then height-1 else if y = height-1 then 0 else y in 
      if not (List.mem (x', y') !pos) then ok := false
    done;
    if !ok then maze else raise (MalformedFile "Invalid wraparound") in 

  let check_doors maze = 
    let rec check_doors' = function 
      | [] -> true
      | (x, y) :: t -> 
        if element_at maze (x, y+1) = CAGE then check_doors' t else false in 
    let ok = check_doors' (find_in DOOR maze) in 
    if ok then maze else 
      raise (MalformedFile "Invalid door configuration") in 

  let check_winnable maze = 
    let comp (x1, y1) (x2, y2) = 
      if x1 = x2 then 
        if y1 < y2 then -1 else if y1 = y2 then 0 else 1
      else 
      if x1 < x2 then -1 else 1 in 
    let reachable' maze start = 
      let height, width = dimension maze in 
      let visited = ref [] in 
      let q = Queue.create () in Queue.push start q;
      while not (Queue.is_empty q) do 
        let pos = Queue.pop q in visited := pos :: !visited;
        let dirs = valid_dirs maze pos in 
        if List.length dirs > 0 then 
          for i = 0 to List.length dirs - 1 do 
            let dir = List.nth dirs i in 
            let dx, dy = Command.dir_to_pos dir in 
            let x, y = pos in 
            let x', y' = Util.modulo (x+dx) width, Util.modulo (y+dy) height in 
            if not (List.mem (x', y') !visited) then Queue.push (x', y') q
          done
      done;
      List.sort_uniq comp !visited in 
    let reachable = reachable' maze (center maze) in 
    let pacdots = List.sort_uniq comp (find_in EMPTY maze) in 
    if reachable = pacdots then maze else 
      raise (MalformedFile "Unwinnable game") in 

  maze |> check_ghosts |> check_corners |> check_wraparounds |> check_doors
  |> check_winnable

(** [csv_to_maze csv] is the pacman maze stored in [csv]. *)
let from_csv csv =  
  Csv.to_array csv |> Array.map (Array.map str_to_element)

let load file = 
  from_csv (Csv.load file) |> rep_ok