(** For this project, we used white box testing to test functions in the
    modules Pacman, Ghost, Command, Maze, State, and Highscores. We
    omitted OUnit testing for printing functions and for functions that
    involved user input, since those are more easily and effectively
    tested manually. We chose to use white box testing primarily because
    there are very specific ghost behaviors for different situations and
    we wanted to make sure that we fully tested every branch. Similarly,
    we wanted to test state updates very thorougly because they
    determine every single frame that is printed in the game; if any
    part of that was incorrect, the entire game would not work properly.
    As a result, we focused primarily on testing these two aspects since
    they are the most complex part of the project, but also added unit
    tests for more basic functions in other modules as a "sanity check,"
    especially since they all play a role in the state update. We also
    included a make bisect target to make sure that we are covering as
    much of each file as possible.*)

open OUnit2
open Pacman
open Ghost
open Command
open Maze
open State
open Highscores

let maze1 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze1.csv")

let maze2 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze3.csv")

let maze3 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze2.csv")

let maze4 = Maze.make_maze (Maze.csv_to_array "maze.csv")

let maze5 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze4.csv")

let maze6 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze5.csv")

let maze7 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze6.csv")

let maze8 =
  Maze.make_maze (Maze.csv_to_array "test_mazes/testmaze7.csv")

let string_of_pos pos =
  match pos with
  | x, y -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ")"

let string_of_dir dir =
  match dir with
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

let string_of_pacman p =
  "Pacman: "
  ^ string_of_pos (Pacman.pos p)
  ^ " "
  ^ string_of_dir (Pacman.dir p)
  ^ " "
  ^ string_of_pos (Pacman.start_pos p)

let string_of_color c =
  match c with
  | Cyan -> "Cyan"
  | Red -> "Red"
  | Pink -> "Pink"
  | Orange -> "Orange"

let string_of_ghost g =
  "Ghost: "
  ^ string_of_pos (Ghost.pos g)
  ^ " "
  ^ string_of_dir (Ghost.dir g)
  ^ " "
  ^ string_of_color (Ghost.color g)
  ^ " "
  ^ string_of_bool (Ghost.active g)
  ^ " "
  ^ string_of_bool (Ghost.scatter g)
  ^ " "
  ^ string_of_int (Ghost.scatter_frames g)
  ^ " "
  ^ string_of_pos (Ghost.start_pos g)

let string_of_list list string_of_elts =
  "["
  ^
  let rec helper list string_of_elts =
    match list with
    | [ e ] -> "\n" ^ string_of_elts e ^ "\n]"
    | e :: t -> "\n" ^ string_of_elts e ^ ";" ^ helper t string_of_elts
    | [] -> "]"
  in
  helper list string_of_elts

let string_of_difficulty diff =
  match diff with Easy -> "Easy" | Normal -> "Normal" | Hard -> "Hard"

let string_of_game_over game_over =
  match game_over with
  | Playing -> "Playing"
  | Won -> "Won"
  | Lost -> "Lost"

let string_of_state st =
  string_of_pacman (State.pacman st)
  ^ "\nGhosts: \n"
  ^ string_of_list (State.ghosts st) string_of_ghost
  ^ "\nLives: "
  ^ string_of_int (State.lives st)
  ^ "\nScore: "
  ^ string_of_int (State.score st)
  ^ "\nPaused: "
  ^ string_of_bool (State.paused st)
  ^ "\nQuit: "
  ^ string_of_bool (State.quit_game st)
  ^ "\nGame state: "
  ^ string_of_game_over (State.game_state st)
  ^ "\nExit tiles: \n"
  ^ string_of_list (State.exit_tiles st) string_of_pos
  ^ "\nMax orbs: "
  ^ string_of_int (State.max_orbs st)
  ^ "\nDifficulty: "
  ^ string_of_difficulty (State.difficulty st)

let rec string_of_leaderboard lb =
  match lb with h :: t -> string_of_list h (fun x -> x) | [] -> ""

let pacman_move_test name start_pos maze move expected_output =
  name >:: fun _ ->
  let pacman = Pacman.make_pacman start_pos Up start_pos in
  assert_equal expected_output
    (Pacman.pos (Pacman.move pacman maze move))
    ~printer:string_of_pos

(* Separate target_pos test functions for each color because they all
   require different parameters. *)
let red_target_pos_test name pacman_pos expected_output =
  name >:: fun _ ->
  let ghost = Ghost.make_ghost (0, 0) Right Red true false 0 (0, 0) in
  let pacman = Pacman.make_pacman pacman_pos Up pacman_pos in
  assert_equal expected_output
    (target_pos ghost pacman [] maze1)
    ~printer:string_of_pos

let cyan_target_pos_test
    name
    pacman_pos
    pacman_dir
    red_ghost_pos
    expected_output =
  name >:: fun _ ->
  let ghost = Ghost.make_ghost (0, 0) Right Cyan true false 0 (0, 0) in
  let red_ghost =
    Ghost.make_ghost red_ghost_pos Right Red true false 0 red_ghost_pos
  in
  let pacman = Pacman.make_pacman pacman_pos pacman_dir pacman_pos in
  assert_equal expected_output
    (target_pos ghost pacman [ red_ghost ] maze1)
    ~printer:string_of_pos

let pink_target_pos_test name pacman_pos pacman_dir expected_output =
  name >:: fun _ ->
  let ghost = Ghost.make_ghost (0, 0) Right Pink true false 0 (0, 0) in
  let pacman = Pacman.make_pacman pacman_pos pacman_dir pacman_pos in
  assert_equal expected_output
    (target_pos ghost pacman [] maze1)
    ~printer:string_of_pos

let orange_target_pos_test
    name
    start_pos
    pacman_pos
    maze
    expected_output =
  name >:: fun _ ->
  let ghost =
    Ghost.make_ghost start_pos Right Orange true false 0 start_pos
  in
  let pacman = Pacman.make_pacman pacman_pos Up pacman_pos in
  assert_equal expected_output
    (target_pos ghost pacman [] maze)
    ~printer:string_of_pos

let scatter_target_pos_test name color maze expected_output =
  name >:: fun _ ->
  let pacman = Pacman.make_pacman (0, 0) Right (0, 0) in
  let ghost = Ghost.make_ghost (5, 5) Right color true true 0 (0, 0) in
  assert_equal expected_output
    (target_pos ghost pacman [] maze)
    ~printer:string_of_pos

let ghost_move_test
    name
    color
    start_pos
    dir
    pacman_pos
    pacman_dir
    maze
    expected_pos
    expected_dir =
  name >:: fun _ ->
  let ghost =
    Ghost.make_ghost start_pos dir color true false 0 start_pos
  in
  let pacman = Pacman.make_pacman pacman_pos pacman_dir pacman_pos in
  assert_equal
    (Ghost.make_ghost expected_pos expected_dir color true false 0
       start_pos)
    (Ghost.move pacman maze [] [] ghost)

(* Separate move test for the Cyan ghost because it requires slightly
   different paraemeters. *)
let cyan_move_test
    name
    start_pos
    dir
    pacman_pos
    pacman_dir
    red_ghost_pos
    maze
    expected_pos
    expected_dir =
  name >:: fun _ ->
  let ghost =
    Ghost.make_ghost start_pos dir Cyan true false 0 start_pos
  in
  let pacman = Pacman.make_pacman pacman_pos pacman_dir pacman_pos in
  let red_ghost =
    Ghost.make_ghost red_ghost_pos Right Red true false 0 red_ghost_pos
  in
  assert_equal
    (Ghost.make_ghost expected_pos expected_dir Cyan true false 0
       start_pos)
    (Ghost.move pacman maze [ red_ghost ] [] ghost)

let is_wall_test name pos maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Maze.is_wall maze pos)
    ~printer:string_of_bool

let is_ghost_box_test name pos maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Maze.is_ghost_box maze pos)
    ~printer:string_of_bool

let is_ghost_box_exit_test name pos maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Maze.is_ghost_box_exit maze pos)
    ~printer:string_of_bool

let has_orb_test name pos maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Maze.has_orb pos maze)
    ~printer:string_of_bool

let has_big_orb_test name pos maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Maze.has_big_orb pos maze)
    ~printer:string_of_bool

let has_won_test name maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Maze.has_won maze)
    ~printer:string_of_bool

let change_pos_test name pos dir expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Maze.change_pos pos dir)
    ~printer:string_of_pos

let orbs_remaining_test name maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Maze.orbs_remaining maze)
    ~printer:string_of_int

let remove_orb_test name pos maze expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Maze.remove_orb pos maze)

let update_test name state_ref command expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( state_ref := State.update !state_ref command;
      !state_ref )
    ~printer:string_of_state

let command_opposite_test name command expected_output =
  name >:: fun _ ->
  assert_equal expected_output (Command.opposite command)

let pacman_reset_pos_test name pos start_pos expected_output =
  let pacman = Pacman.make_pacman pos Up start_pos in
  name >:: fun _ ->
  assert_equal expected_output (Pacman.pos (Pacman.reset_pos pacman))

let highscores_addscore_test name fname user score expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( Highscores.add_score fname user score;
      Highscores.load fname )
    ~printer:string_of_leaderboard

let highscores_sort_test name fname expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    ( Highscores.sort_board fname;
      Highscores.load fname )
    ~printer:string_of_leaderboard

let pacman_tests =
  [
    pacman_move_test "Pacman start at (1, 1), move down" (1, 1) maze1
      Down (1, 2);
    pacman_move_test "Pacman start at (1, 1), move up" (1, 1) maze1 Up
      (1, 1);
    pacman_move_test "Pacman start at (1, 1), move left" (1, 1) maze1
      Left (1, 1);
    pacman_move_test "Pacman start at (1, 1), move right" (1, 1) maze1
      Right (1, 1);
    pacman_move_test "Pacman start at (3, 1), move right" (3, 1) maze1
      Right (4, 1);
    pacman_move_test "Pacman start at (4, 1), move left" (4, 1) maze1
      Left (3, 1);
    pacman_move_test "Pacman start at (1, 2), move right" (1, 2) maze1
      Right (1, 1);
    pacman_reset_pos_test
      "Resetting the position of Pac-Man with pos (3, 4) and start_pos \
       (1, 2) moves Pac-Man back to (1, 2)."
      (3, 4) (1, 2) (1, 2);
    pacman_reset_pos_test
      "Resetting the position of Pac-Man with pos (0, 0) and start_pos \
       (0, 0) moves Pac-Man back to (0, 0)."
      (0, 0) (0, 0) (0, 0);
    pacman_reset_pos_test
      "Resetting the position of Pac-Man with pos (0, 0) and start_pos \
       (-1, -2) moves Pac-Man back to (-1, -2)."
      (0, 0) (-1, -2) (-1, -2);
    pacman_reset_pos_test
      "Resetting the position of Pac-Man with pos (99999, 99999) and \
       start_pos (-99999, -99999) moves Pac-Man back to (-99999, \
       -99999)."
      (99999, 99999) (-99999, -99999) (-99999, -99999);
  ]

let ghost_tests =
  [
    (* target_pos tests *)
    red_target_pos_test
      "Red ghost target_pos with Pacman at (3, 3) is (3, 3)." (3, 3)
      (3, 3);
    red_target_pos_test
      "Red ghost target_pos with Pacman at (5, -2) is (5, -2)." (5, -2)
      (5, -2);
    red_target_pos_test
      "Red ghost target_pos with Pacman at (0, 0) is (0, 0)." (0, 0)
      (0, 0);
    red_target_pos_test
      "Red ghost target_pos with Pacman at (-3, 2) is (-3, 2)." (-3, 2)
      (-3, 2);
    cyan_target_pos_test
      "Cyan ghost target_pos with Pacman at (0, 0) moving right and \
       red ghost at (3, 3) is (2, -2)."
      (0, 0) Right (3, 3) (1, -3);
    cyan_target_pos_test
      "Cyan ghost target_pos with Pacman at (0, 0) moving down and red \
       ghost at (3, 3) is (-3, 1)."
      (0, 0) Down (3, 3) (-3, 1);
    cyan_target_pos_test
      "Cyan ghost target_pos with Pacman at (0, 0) moving up and red \
       ghost at (3, 3) is (-3, -7)."
      (0, 0) Up (3, 3) (-3, -7);
    cyan_target_pos_test
      "Cyan ghost target_pos with Pacman at (0, 0) moving left and red \
       ghost at (3, 3) is (-7, -3)."
      (0, 0) Left (3, 3) (-7, -3);
    pink_target_pos_test
      "Pink ghost target_pos with Pacman at (0, 0) moving right is (4, \
       0)."
      (0, 0) Right (4, 0);
    pink_target_pos_test
      "Pink ghost target_pos with Pacman at (0, 0) moving left is (-4, \
       0)."
      (0, 0) Left (-4, 0);
    pink_target_pos_test
      "Pink ghost target_pos with Pacman at (0, 0) moving up is (0, \
       -4)."
      (0, 0) Up (0, -4);
    pink_target_pos_test
      "Pink ghost target_pos with Pacman at (0, 0) moving down is (0, \
       4)."
      (0, 0) Down (0, 4);
    (* Testing orange ghost target_pos when Pacman is within 8 tiles. *)
    orange_target_pos_test
      "Orange ghost target_pos with ghost on (0, 0) and Pacman at (8, \
       0) in maze1 is (0, 7)."
      (0, 0) (8, 0) maze1 (0, 7);
    orange_target_pos_test
      "Orange ghost target_pos with ghost on (0, 0) and Pacman at (5, \
       5) in maze1 is (0, 7)."
      (0, 0) (5, 5) maze1 (0, 7);
    (* Testing orange ghost target_pos when Pacman is more that 8 tiles
       away. *)
    orange_target_pos_test
      "Orange ghost target_pos with ghost on (0, 0) and Pacman at (9, \
       0) in maze1 is (9, 0)."
      (0, 0) (9, 0) maze1 (9, 0);
    orange_target_pos_test
      "Orange ghost target_pos with ghost on (0, 0) and Pacman at (6, \
       6) in maze1 is (6, 6)."
      (0, 0) (6, 6) maze1 (6, 6);
    scatter_target_pos_test "Pink ghost scatters to (0, 0) in maze1."
      Pink maze1 (0, 0);
    scatter_target_pos_test "Pink ghost scatters to (0, 0) in maze2."
      Pink maze2 (0, 0);
    scatter_target_pos_test "Orange ghost scatters to (0, 7) in maze1."
      Orange maze1 (0, 7);
    scatter_target_pos_test "Orange ghost scatters to (0, 23) in maze2."
      Orange maze2 (0, 23);
    scatter_target_pos_test "Cyan ghost scatters to (12, 7) in maze1."
      Cyan maze1 (12, 7);
    scatter_target_pos_test "Cyan ghost scatters to (27, 23) in maze2."
      Cyan maze2 (27, 23);
    scatter_target_pos_test "Red ghost scatters to (12, 0) in maze1."
      Red maze1 (12, 0);
    scatter_target_pos_test "Red ghost scatters to (27, 0) in maze2."
      Red maze2 (27, 0);
    (* move_tests: 1 test for keeping the same direction, 1 test for
       changing directions, 1 test for dead ends. *)
    ghost_move_test
      "Red ghost at (2, 2) moving down in maze3 with Pacman on (3, 5) \
       moving up moves down to (2, 3)."
      Red (2, 2) Down (3, 5) Up maze3 (2, 3) Down;
    ghost_move_test
      "Red ghost at (2, 2) moving right in maze3 with Pacman on (0, 1) \
       moving up moves up to (2, 1)."
      Red (2, 2) Right (0, 1) Up maze3 (2, 1) Up;
    ghost_move_test
      "Red ghost at (1, 2) moving left in maze3 with Pacman on (0, 2) \
       moving up moves right to (2, 2)."
      Red (1, 2) Left (0, 2) Up maze3 (2, 2) Right;
    ghost_move_test
      "Pink ghost at (2, 2) moving down in maze3 with Pacman on (0, 1) \
       moving left moves left to (1, 2)."
      Pink (2, 2) Down (0, 1) Left maze3 (1, 2) Left;
    ghost_move_test
      "Pink ghost at (2, 2) moving down in maze3 with Pacman on (0, 1) \
       moving down moves to down (2, 3)."
      Pink (2, 2) Down (0, 1) Down maze3 (2, 3) Down;
    ghost_move_test
      "Pink ghost at (1, 2) moving left in maze3 with Pacman on (0, 1) \
       moving left moves right to (2, 2)."
      Pink (1, 2) Left (0, 1) Left maze3 (2, 2) Right;
    ghost_move_test
      "Orange ghost at (2, 2) moving down in maze3 with Pacman on (12, \
       0) moving left moves right to (3, 2)."
      Orange (2, 2) Down (12, 0) Left maze3 (3, 2) Right;
    ghost_move_test
      "Orange ghost at (2, 2) moving down in maze3 with Pacman on (0, \
       12) moving left moves down to (2, 3)."
      Orange (2, 2) Down (0, 12) Left maze3 (2, 3) Down;
    ghost_move_test
      "Orange ghost at (1, 2) moving left in maze3 with Pacman on (12, \
       0) moving left moves right to (2, 2)."
      Orange (1, 2) Left (12, 0) Left maze3 (2, 2) Right;
    cyan_move_test
      "Cyan ghost at (2, 2) moving right in maze3 with Pacman on (2, \
       5) moving right and red ghost on (-3, 6) moves right to (3, 2)"
      (2, 2) Right (2, 5) Right (-3, 6) maze3 (3, 2) Right;
    cyan_move_test
      "Cyan ghost at (2, 2) moving right in maze3 with Pacman on (2, \
       5) moving up and red ghost on (2, 6) moves up to (2, 1)"
      (2, 2) Right (2, 5) Up (2, 6) maze3 (2, 1) Up;
    cyan_move_test
      "Cyan ghost at (1, 2) moving left in maze3 with Pacman on (2, 5) \
       moving up and red ghost on (2, 6) moves right to (2, 2)"
      (1, 2) Left (2, 5) Up (2, 6) maze3 (2, 2) Right;
  ]

let maze_tests =
  [
    is_wall_test "is_wall (0, 0) maze1 = true" (0, 0) maze1 true;
    is_wall_test "is_wall (2, 1) maze1 = true" (2, 1) maze1 true;
    is_wall_test "is_wall (-1, 1) maze1 = true" (-1, 1) maze1 false;
    is_wall_test "is_wall (1, 1) maze1 = false" (1, 1) maze1 false;
    is_wall_test "is_wall (1, 2) maze1 = false" (1, 2) maze1 false;
    is_wall_test "is_wall (13, 9) maze4 = false" (13, 9) maze4 false;
    is_wall_test "is_wall (13, 10) maze4 = false" (13, 10) maze4 false;
    is_wall_test "is_wall (12, 9) maze4 = true" (12, 9) maze4 true;
    is_ghost_box_test "is_ghost_box (0, 0) maze4 = false" (0, 0) maze4
      false;
    is_ghost_box_test "is_ghost_box (3, 5) maze4 = false" (3, 5) maze4
      false;
    is_ghost_box_test "is_ghost_box (13, 10) maze4 = true" (13, 10)
      maze4 true;
    is_ghost_box_test "is_ghost_box (13, 11) maze4 = true" (13, 11)
      maze4 true;
    is_ghost_box_test "is_ghost_box (13, 9) maze4 = false" (13, 9) maze4
      false;
    is_ghost_box_test "is_ghost_box (-1, -3) maze4 = false" (-1, -3)
      maze4 false;
    is_ghost_box_exit_test "is_ghost_box_exit (0, 0) maze4 = false"
      (0, 0) maze4 false;
    is_ghost_box_exit_test "is_ghost_box_exit (13, 9) maze4 = true"
      (13, 9) maze4 true;
    is_ghost_box_exit_test "is_ghost_box_exit (14, 9) maze4 = true"
      (14, 9) maze4 true;
    is_ghost_box_exit_test "is_ghost_box_exit (14, 10) maze4 = false"
      (14, 10) maze4 false;
    is_ghost_box_exit_test "is_ghost_box_exit (14, 8) maze4 = false"
      (14, 8) maze4 false;
    has_orb_test "has_orb (0, 0) maze4 = false" (0, 0) maze4 false;
    has_orb_test "has_orb (0, 1) maze4 = false" (0, 1) maze4 false;
    has_orb_test "has_orb (1, 1) maze4 = true" (1, 1) maze4 true;
    has_orb_test "has_orb (1, 2) maze4 = true" (1, 2) maze4 true;
    has_orb_test "has_orb (13, 11) maze4 = false" (13, 11) maze4 false;
    has_orb_test "has_orb (13, 9) maze4 = false" (13, 9) maze4 false;
    has_orb_test "has_orb (13, 8) maze4 = true" (13, 8) maze4 true;
    has_big_orb_test "has_big_orb (0, 0) maze4 = false" (0, 0) maze4
      false;
    has_big_orb_test "has_big_orb (0, 1) maze4 = false" (0, 1) maze4
      false;
    has_big_orb_test "has_big_orb (1, 1) maze4 = false" (1, 1) maze4
      false;
    has_big_orb_test "has_big_orb (13, 11) maze4 = false" (13, 11) maze4
      false;
    has_big_orb_test "has_big_orb (13, 9) maze4 = false" (13, 9) maze4
      false;
    has_big_orb_test "has_big_orb (13, 8) maze4 = false" (13, 8) maze4
      false;
    has_big_orb_test "has_big_orb (1, 3) maze4 = true" (1, 3) maze4 true;
    has_big_orb_test "has_big_orb (1, 18) maze4 = true" (1, 18) maze4
      true;
    has_won_test "has_won maze1 = false" maze1 false;
    has_won_test "has_won maze2 = false" maze2 false;
    has_won_test "has_won maze3 = false" maze3 false;
    has_won_test "has_won maze4 = false" maze4 false;
    has_won_test "has_won maze5 = false" maze5 false;
    has_won_test "has_won maze6 = false" maze6 false;
    has_won_test "has_won maze7 = true" maze7 true;
    change_pos_test "change_pos (0, 0) Right = (1, 0)" (0, 0) Right
      (1, 0);
    change_pos_test "change_pos (0, 0) Down = (0, 1)" (0, 0) Down (0, 1);
    change_pos_test "change_pos (0, 0) Left = (-1, 0)" (0, 0) Left
      (-1, 0);
    change_pos_test "change_pos (0, 0) Up = (0, -1)" (0, 0) Up (0, -1);
    orbs_remaining_test "orbs_remaining maze1 = " maze1 42;
    orbs_remaining_test "orbs_remaining maze2 = " maze2 279;
    orbs_remaining_test "orbs_remaining maze3 = " maze3 5;
    orbs_remaining_test "orbs_remaining maze4 = " maze4 259;
    orbs_remaining_test "orbs_remaining maze5 = 1" maze5 1;
    orbs_remaining_test "orbs_remaining maze6 = 1" maze6 1;
    orbs_remaining_test "orbs_remaining maze7 = 0" maze7 0;
    orbs_remaining_test "orbs_remaining maze8 = " maze8 39;
    remove_orb_test "remove_orb (0, 0) maze1 is maze1" (0, 0) maze1
      maze1;
    remove_orb_test "remove_orb (1, 1) maze6 is maze7" (1, 1) maze6
      maze7;
    remove_orb_test "remove_orb (1, 1) maze5 is maze7" (1, 1) maze5
      maze7;
    remove_orb_test "remove_orb (13, 9) maze4 is maze4" (13, 9) maze4
      maze4;
  ]

let command_tests =
  [
    command_opposite_test "opposite Up is Down" Up Down;
    command_opposite_test "opposite Down is Up" Down Up;
    command_opposite_test "opposite Left is Right" Left Right;
    command_opposite_test "opposite Right is Left" Right Left;
  ]

(* For testing mazes after removing multiple orbs. *)
let rec remove_orbs poslist maze =
  match poslist with
  | pos :: t -> remove_orbs t (Maze.remove_orb pos maze)
  | [] -> maze

let state1 =
  ref
    (State.init_state
       (Maze.csv_to_array "test_mazes/testmaze1.csv")
       Easy)

let state2 =
  ref
    (State.init_state
       (Maze.csv_to_array "test_mazes/testmaze7.csv")
       Easy)

let state_tests =
  [
    (* Testing updates in a simple maze with only walls, small orbs, and
       Pac-Man. *)
    update_test "1st update state1, command (Move Up)" state1 (Move Up)
      (State.make_state maze1
         (Pacman.make_pacman (1, 1) Right (1, 1))
         [] 3 0 false false Playing [] 42 Easy);
    update_test "2nd update state1, command (Move Down)" state1
      (Move Down)
      (State.make_state
         (remove_orbs [ (1, 2) ] maze1)
         (Pacman.make_pacman (1, 2) Down (1, 1))
         [] 3 10 false false Playing [] 42 Easy);
    update_test "3rd update state1, command (Move Down)" state1
      (Move Down)
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3) ] maze1)
         (Pacman.make_pacman (1, 3) Down (1, 1))
         [] 3 20 false false Playing [] 42 Easy);
    update_test "4th update state1, command (Move Left)" state1
      (Move Left)
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4) ] maze1)
         (Pacman.make_pacman (1, 4) Down (1, 1))
         [] 3 30 false false Playing [] 42 Easy);
    update_test "5th update state1, command Pause" state1 Pause
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4) ] maze1)
         (Pacman.make_pacman (1, 4) Down (1, 1))
         [] 3 30 true false Playing [] 42 Easy);
    update_test "6th update state1, command (Move Left)" state1
      (Move Left)
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4) ] maze1)
         (Pacman.make_pacman (1, 4) Down (1, 1))
         [] 3 30 true false Playing [] 42 Easy);
    update_test "7th update state1, command (Move Down)" state1
      (Move Down)
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4) ] maze1)
         (Pacman.make_pacman (1, 4) Down (1, 1))
         [] 3 30 true false Playing [] 42 Easy);
    update_test "8th update state1, command Pause" state1 Pause
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4) ] maze1)
         (Pacman.make_pacman (1, 4) Down (1, 1))
         [] 3 30 false false Playing [] 42 Easy);
    update_test "9th update state1, command (Move Right)" state1
      (Move Right)
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4); (2, 4) ] maze1)
         (Pacman.make_pacman (2, 4) Right (1, 1))
         [] 3 40 false false Playing [] 42 Easy);
    update_test "10th update state1, command Quit" state1 Quit
      (State.make_state
         (remove_orbs [ (1, 2); (1, 3); (1, 4); (2, 4) ] maze1)
         (Pacman.make_pacman (2, 4) Right (1, 1))
         [] 3 40 false true Playing [] 42 Easy);
    (* Testing updates in a slightly more complicated maze with a small
       ghost box, a red ghost, and 1 big orb. Also tests ghost collision
       with Pac-Man. *)
    update_test "1st update state2, command (Move Down)" state2
      (Move Down)
      (State.make_state
         (remove_orbs [ (11, 2) ] maze8)
         (Pacman.make_pacman (11, 2) Down (11, 1))
         [ Ghost.make_ghost (8, 4) Left Red true false 0 (9, 4) ]
         3 10 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "2nd update state2, command (Move Right)" state2
      (Move Right)
      (State.make_state
         (remove_orbs [ (11, 2); (11, 3) ] maze8)
         (Pacman.make_pacman (11, 3) Down (11, 1))
         [ Ghost.make_ghost (7, 4) Left Red true false 0 (9, 4) ]
         3 20 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "3rd update state2, command (Move Left)" state2
      (Move Left)
      (State.make_state
         (remove_orbs [ (11, 2); (11, 3); (11, 4) ] maze8)
         (Pacman.make_pacman (11, 4) Down (11, 1))
         [ Ghost.make_ghost (6, 4) Left Red true false 0 (9, 4) ]
         3 30 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "4th update state2, command (Move Up)" state2 (Move Up)
      (State.make_state
         (remove_orbs [ (11, 2); (11, 3); (11, 4) ] maze8)
         (Pacman.make_pacman (11, 3) Up (11, 1))
         [ Ghost.make_ghost (5, 4) Left Red true false 0 (9, 4) ]
         3 30 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "5th update state2, command (Move Up)" state2 (Move Up)
      (State.make_state
         (remove_orbs [ (11, 2); (11, 3); (11, 4) ] maze8)
         (Pacman.make_pacman (11, 2) Up (11, 1))
         [ Ghost.make_ghost (4, 4) Left Red true false 0 (9, 4) ]
         3 30 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "6th update state2, command (Move Up)" state2 (Move Up)
      (State.make_state
         (remove_orbs [ (11, 2); (11, 3); (11, 4) ] maze8)
         (Pacman.make_pacman (11, 1) Up (11, 1))
         [ Ghost.make_ghost (4, 3) Up Red true false 0 (9, 4) ]
         3 30 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "7th update state2, command (Move Left)" state2
      (Move Left)
      (State.make_state
         (remove_orbs [ (11, 2); (11, 3); (11, 4); (10, 1) ] maze8)
         (Pacman.make_pacman (10, 1) Left (11, 1))
         [ Ghost.make_ghost (4, 2) Up Red true false 0 (9, 4) ]
         3 40 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "8th update state2, command (Move Left)" state2
      (Move Left)
      (State.make_state
         (remove_orbs
            [ (11, 2); (11, 3); (11, 4); (10, 1); (9, 1) ]
            maze8)
         (Pacman.make_pacman (9, 1) Left (11, 1))
         [ Ghost.make_ghost (4, 1) Down Red true true 0 (9, 4) ]
         3 90 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "9th update state2, command (Move Down)" state2
      (Move Down)
      (State.make_state
         (remove_orbs
            [ (11, 2); (11, 3); (11, 4); (10, 1); (9, 1); (9, 2) ]
            maze8)
         (Pacman.make_pacman (9, 2) Down (11, 1))
         [ Ghost.make_ghost (5, 1) Right Red true true 1 (9, 4) ]
         3 100 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "10th update state2, command (Move Left)" state2
      (Move Left)
      (State.make_state
         (remove_orbs
            [
              (11, 2); (11, 3); (11, 4); (10, 1); (9, 1); (9, 2); (8, 2);
            ]
            maze8)
         (Pacman.make_pacman (8, 2) Left (11, 1))
         [ Ghost.make_ghost (6, 1) Right Red true true 2 (9, 4) ]
         3 110 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "11th update state2, command (Move Left)" state2
      (Move Left)
      (State.make_state
         (remove_orbs
            [
              (11, 2);
              (11, 3);
              (11, 4);
              (10, 1);
              (9, 1);
              (9, 2);
              (8, 2);
              (7, 2);
            ]
            maze8)
         (Pacman.make_pacman (7, 2) Left (11, 1))
         [ Ghost.make_ghost (7, 1) Right Red true true 3 (9, 4) ]
         3 120 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
    update_test "12th update state2, command (Move Up)" state2 (Move Up)
      (State.make_state
         (remove_orbs
            [
              (11, 2);
              (11, 3);
              (11, 4);
              (10, 1);
              (9, 1);
              (9, 2);
              (8, 2);
              (7, 2);
              (7, 1);
            ]
            maze8)
         (Pacman.make_pacman (7, 1) Up (11, 1))
         [ Ghost.make_ghost (9, 4) Down Red true false 0 (9, 4) ]
         3 330 false false Playing [ (10, 4); (7, 4) ] 39 Easy);
  ]

let test_file_1 = "leaderboards/add_tests.csv"

let leaderboard1 = Csv.save test_file_1 []

let test_file_2 = "leaderboards/sort_tests.csv"

let leaderboard1 =
  Csv.save test_file_2
    [
      [ "Tanvi"; "500" ];
      [ "Matt"; "1600" ];
      [ "Rena"; "230" ];
      [ "Kate"; "100" ];
    ]

let highscores_tests =
  [
    highscores_addscore_test "add to empty file" test_file_1 "Tanvi"
      "400" [ [ "Tanvi"; "400" ] ];
    highscores_addscore_test "add to file with entries" test_file_1
      "Matt" "600"
      [ [ "Tanvi"; "400" ]; [ "Matt"; "600" ] ];
    highscores_addscore_test "add a long name to file" test_file_1
      "Prof.Clarkson" "1000"
      [
        [ "Tanvi"; "400" ]; [ "Matt"; "600" ]; [ "Prof.Clark"; "1000" ];
      ];
    highscores_sort_test "sort an unordered file" test_file_2
      [
        [ "Matt"; "1600" ];
        [ "Tanvi"; "500" ];
        [ "Rena"; "230" ];
        [ "Kate"; "100" ];
      ];
    highscores_sort_test "sort an ordered file" test_file_2
      [
        [ "Matt"; "1600" ];
        [ "Tanvi"; "500" ];
        [ "Rena"; "230" ];
        [ "Kate"; "100" ];
      ];
  ]

let suite =
  "Test suite for Pac-Man"
  >::: List.flatten
         [
           pacman_tests;
           ghost_tests;
           maze_tests;
           command_tests;
           state_tests;
           highscores_tests;
         ]

let _ = run_test_tt_main suite
