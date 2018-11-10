open OUnit2
open State
open Command


let maze = Maze.load "games/3110.csv"
let state = State.init_state maze
let pacman = Pacman.init 3 (Maze.center maze) UP
let state2 = State.next maze state (MOVE UP)
let state3 = State.next maze state2 (MOVE UP)
let state4 = State.next maze state3 (MOVE UP)
let state5 = State.next maze state4 (MOVE UP)

let invalid_maze_3ghosts = Maze.load "IllegalGames/3Ghosts.csv"
let invalid_maze_BlockedCorners = Maze.load "IllegalGames/BlockedCorners.csv"
let invalid_maze_NonRectangular= Maze.load "IllegalGames/NonRectangular.csv"

(** [make_pacman_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [State.pacman state].
*)
let make_pacman_test 
    (name: string)
    (state: t)
    (expected_output: Pacman.t) : test = 
  name >:: (fun _ -> assert_equal expected_output (State.pacman state))

(** [make_ghost_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [List.length (State.ghosts state)].
*)
let make_ghost_test 
    (name: string)
    (state: t)
    (expected_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output 
               (List.length (State.ghosts state)))

(** [make_score_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [State.score state].
*)
let make_score_test 
    (name: string)
    (state: t)
    (expected_output: int) : test = 
  name >:: (fun _ -> assert_equal expected_output (State.score state))

(** [make_dead_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [State.dead state].
*)
let make_dead_test 
    (name: string)
    (state: t)
    (expected_output:bool) : test = 
  name >:: (fun _-> assert_equal expected_output (State.dead state))

(** [make_beat_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [State.beat_level state].
*)
let make_beat_level_test 
    (name: string)
    (state: t)
    (expected_output:bool) : test = 
  name >:: (fun _-> assert_equal expected_output (State.beat_level state))

(** [make_respawn_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [State.respawn maze state].
*)
let make_respawn_test 
    (name: string)
    (maze: Maze.t)
    (state: t)
    (expected_output:Pacman.t) : test = 
  name >:: (fun _-> assert_equal expected_output 
               (State.pacman (State.respawn maze state)))

(** [make_next_test] constructs an OUnit test named [name] that asserts
    the equality of [expected_output] with [State.next maze state cmm].
*)
let make_next_test 
    (name: string)
    (maze: Maze.t)
    (state: t)
    (cmm: Command.cmd)
    (expected_output:Pacman.t) : test = 
  name >:: (fun _-> assert_equal expected_output 
               (State.pacman (State.next maze state cmm)))

let blocked_corner_maze = 
let name = "" in
let blocked_corner_maze' = 
  try Some (Maze.rep_ok invalid_maze_BlockedCorners) 
  with  Maze.MalformedFile "Corners need to be empty"-> None in
  name >:: (fun _-> assert (blocked_corner_maze' == None))

let only_3ghost_maze = 
let name = "" in
let only_3ghost_maze' = 
  try Some (Maze.rep_ok invalid_maze_3ghosts) 
  with  Maze.MalformedFile "there needs to be exactly 4 ghosts"-> None in
  name >:: (fun _-> assert (only_3ghost_maze' == None))

let state_tests =[
  make_pacman_test "pac1" state pacman;
  make_ghost_test "numghost1" state 4;
  make_score_test "startscore1" state (-10);
  make_score_test "nextscore1" state2 (0);
  make_score_test "nextscore2" state3 (10);
  make_score_test "nextscore3" state4 (20);
  make_dead_test "dead1" state false;
  make_beat_level_test "beatlevel1" state false;
  make_respawn_test "respawn1" maze state (Pacman.init 2 (7,5) UP);
  make_next_test "next1" maze state (MOVE UP) (Pacman.init 3 (7,4) UP);
  make_next_test "next2" maze state2 (MOVE UP) (Pacman.init 3 (7,3) UP);
  make_next_test "next3" maze state3 (MOVE UP) (Pacman.init 3 (7,2) UP);
  make_next_test "next5" maze state (MOVE DOWN) (Pacman.init 3 (7,6) DOWN);
  make_next_test "next6" maze state2 (MOVE DOWN) (Pacman.init 3 (7,5) DOWN);
  make_next_test "next7" maze state3 (MOVE DOWN) (Pacman.init 3 (7,4) DOWN);
]

let maze_tests =[
  blocked_corner_maze;
  only_3ghost_maze
]

let suite =
  "test suite for A7"  >::: List.flatten [
    state_tests;
    maze_tests;
  ]

let _ = run_test_tt_main suite  

