include Command

(** Raised when a function times out. *)
exception Timeout

(** Abstract type representing a Pacman game. *)
type game = {
  maze: Maze.t;
  file_name: string;
  mutable time: int;
  mutable state: State.t;
  mutable level: int
}

(** [printer game] is a list specifying how to render each coordinate of the
    game. Each element is a pair (c, v) where c is the color, and v is the
    string to be printed on the terminal. *)
let printer game = 
  let maze = game.maze |> Maze.to_printer in 
  let state = game.state in 
  State.add_pac_dots maze state;
  State.add_power_pellets maze state;
  State.add_fruits maze state;
  State.add_ghosts maze state;
  State.add_pacman maze state;
  Util.arr_to_list maze

let rec render_row row = 
  match row with 
  | [] -> print_string "\n"
  | (styles, str) :: t -> ANSITerminal.(print_string styles str); render_row t

(** [erase] is a string that deletes everything currently displayed on the 
    terminal. This is used to continuously update the game screen. *)
let erase = 
  let channel = Unix.open_process_in "clear" in
  let str = input_line channel in
  ignore (Unix.close_process_in channel);
  str

(** [instructions] is the set of instructions to be printed at the top of the
    game screen. *)
let instructions = 
  "Instructions:\n" ^
  "Use (w) up (a) left (s) down (d) right to navigate.\n" ^
  "Press (p) to pause and (q) to quit.\n\n"

(** TODO: Document *)
(** [render_game game] prints the game described by [game] to the terminal. *)
let render_game ?(debug: bool=false) game = 
  print_string erase;
  print_endline instructions;
  let rec render_game' = function
    | [] -> print_string "\n"
    | h :: t -> render_row h; render_game' t
  in printer game |> render_game';
  (** Print positions of pacman/ghosts when in debug mode *)
  if debug then begin 
    print_endline ("Pacman: " ^ (game.state |> State.pacman |> Pacman.position 
                                 |> Util.position_to_string));
    print_string "Ghosts: ";
    for i = 0 to 3 do
      print_string ((List.nth (game.state |> State.ghosts) i |> Ghosts.position 
                     |> Util.position_to_string) ^ " ");
    done; 
    print_endline ("\nDead: " ^ (game.state |> State.dead |> string_of_bool));
    print_string "\n\n";
  end;
  let n_lives = game.state |> State.pacman |> Pacman.lives |> string_of_int in 
  print_endline ("Lives: " ^ n_lives);
  let score = game.state |> State.score |> string_of_int in 
  print_endline ("Your Score: " ^ score)

(** [command] and [buffer] are global variables used to monitor game state.
    [command] stores the most recently issued command.
    [buffer] is a byte array used to store the most recently pressed key. *)
let command = ref (MOVE UP)
let buffer = Bytes.create 1

(** [listen ()] waits for a key to be pressed and updates [command] based on
    key. The W/A/S/D keys are used to move Mr. Pacman, P is used to pause the 
    game, and Q is used to quit the game. All other keys have no effect. *)
let listen () =
  let status = Unix.tcgetattr Unix.stdin in 
  Unix.(tcsetattr stdin TCSADRAIN {status with c_icanon=false; c_echo=false});
  ignore Unix.(read stdin buffer 0 1);
  match Bytes.to_string buffer with 
  | "W" | "w" -> (MOVE UP)
  | "S" | "s" -> (MOVE DOWN)
  | "A" | "a" -> (MOVE LEFT)
  | "D" | "d" -> (MOVE RIGHT)
  | "P" | "p" -> PAUSE
  | "Q" | "q" -> QUIT
  | _ -> !command

(** [listen_timeout t waits for a key to be pressed and updates [command] 
    based on that key. If no key is pressed within t seconds, [command] 
    remains unchanged. *)
let listen_timeout t = 
  let t' = match !command with 
    | MOVE UP | MOVE DOWN -> t *. 1.3
    | _ -> t
  in 
  try 
    let t_start = Unix.gettimeofday () in 
    Sys.(set_signal sigalrm (Signal_handle (fun _ -> raise Timeout)));
    ignore (Unix.(setitimer ITIMER_REAL {it_interval=t'; it_value=t'}));
    command := listen ();
    ignore (Unix.alarm 0);
    Sys.(set_signal sigalrm Signal_default);
    let elapsed = Unix.gettimeofday () -. t_start in 
    if elapsed < t' then Unix.sleepf (t' -. elapsed)
  with Timeout -> 
    ignore (Unix.alarm 0);
    Sys.(set_signal sigalrm Signal_default)

(** [new_game f] is the game specified by file [f]. If [f] is not found,
    a new file oath is requested in the terminal. *)
let rec new_game f = 
  try 
    let maze = Maze.load f in 
    {
      maze = maze;
      file_name = f; 
      time = 0;
      state = State.init_state maze;
      level = 1;
    }
  with 
  | Sys_error _ -> 
    print_string "File not found, try again.\n> ";
    new_game (read_line ())
  | Maze.MalformedFile s -> 
    print_string (s ^ ". Please select a different file.\n> ");
    new_game (read_line ())

(** [play debug game] plays the game specified by [game]. If [debug], the game
    waits for a key to be pressed for every single game tick. *)
let play ?(debug: bool=false) game = 
  command := MOVE (game.state |> State.pacman |> Pacman.direction);
  let playing = ref true in 
  while !playing do 
    render_game game;
    let t = 0.12 +. if debug then 1000. else 0. in
    listen_timeout (t -. 0.01 *. float_of_int game.level);
    if State.game_over game.state then command := QUIT;
    match !command with
    | MOVE dir ->
      game.state <- State.next game.maze game.state (MOVE dir);
      (** When pacman is dead *)
      if State.dead game.state then 
        (** TODO: Add animation for dead pacman *) 
        (render_game game;
         Unix.sleep 2;
         game.state <- State.respawn game.maze game.state;
         command := MOVE (game.state |> State.pacman |> Pacman.direction));
      (** When level is over *)
      if State.beat_level game.state then 
        (render_game game;
         Unix.sleep 2;
         game.state <- State.next_level game.maze game.state;
         game.level <- game.level + 1;
         command := MOVE (game.state |> State.pacman |> Pacman.direction));
    | PAUSE ->
      print_endline "\nPAUSED. Double-tap any key to resume.";
      ignore (listen ());
    | QUIT ->
      let status = Unix.tcgetattr Unix.stdin in 
      Unix.(tcsetattr stdin TCSADRAIN {status with c_icanon=true; c_echo=true});
      Leaderboard.load_leaderboard (State.score game.state) 
        (State.levels_completed game.state) game.file_name; 
      print_endline "\nThanks for playing!";
      playing := false
  done

(** [main ()] prompts the user for a game file and plays that game, 
    re-prompting if the given file is not found. *)
let main () = 
  ANSITerminal.(print_string [red] "Select a game file.\n");
  (** TODO: List files in ./games directory (?) *)
  print_string "> ";
  match read_line () with 
  | exception End_of_file -> ()
  | file -> let game = new_game file in play ~debug:false game

let () = main ()
