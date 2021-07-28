open Command
open Highscores

(** Time for each frame in seconds. *)
let time_per_frame = 1

(** Clear the terminal. *)
let clear () = ignore (Sys.command "clear")

(** The original settings for the terminal. *)
let og_terminal_settings = Unix.tcgetattr Unix.stdin

(** Change the terminal settings to single character input. *)
let single_char_input () =
  Unix.tcsetattr Unix.stdin TCSANOW
    { og_terminal_settings with c_icanon = false }

(** Revert the terminal to its original settings. *)
let revert_terminal () =
  Unix.tcsetattr Unix.stdin TCSANOW og_terminal_settings

(** Exception for when a function execution times out. *)
exception Timeout

(* Helper for timeout; raise exception Timeout when the system receives
   the virtual alarm signal. *)
let set_system_timeout () =
  let handle_sigalrm signal =
    if signal = Sys.sigalrm then raise Timeout else ()
  in
  Sys.set_signal Sys.sigalrm (Signal_handle handle_sigalrm)

(** Tries to execute f x within [max] seconds. If f does not finish
    executing, then return [default]. *)
let timeout f x max =
  set_system_timeout ();
  ignore (Unix.alarm max);
  let value = f x in
  ignore (Unix.alarm 0);
  value

(** Prompts the user for a single key input and then returns the command
    represented by the input. *)
let user_input () =
  single_char_input ();
  let command =
    let input = input_char stdin in
    match input with
    | 'w' -> Some (Move Up)
    | 'a' -> Some (Move Left)
    | 's' -> Some (Move Down)
    | 'd' -> Some (Move Right)
    | 'p' -> Some Pause
    | 'q' -> Some Quit
    | _ -> None
  in
  revert_terminal ();
  command

let rec get_difficulty_helper () =
  let str = read_line () in
  try State.difficulty_of_string str
  with Failure _ ->
    print_string
      "\nPlease enter a valid difficulty: easy, normal, or hard. \n> ";
    get_difficulty_helper ()

(** Prompts the user for a line input and returns the difficulty
    represented by the input. Prompts again if the input does not
    represent any difficulty. *)
let rec get_difficulty () =
  clear ();
  print_endline
    "What difficulty would you like to play on?\n\
    \ 1. Easy\n\
    \ 2. Normal\n\
    \ 3. Hard";
  print_string "> ";
  get_difficulty_helper ()

(** Same as get_difficulty() but with a different printed message. *)
let rec select_level () =
  print_endline
    "What difficulty would you like to view the leaderbord for?\n\
    \ 1. Easy\n\
    \ 2. Normal\n\
    \ 3. Hard";
  print_string "> ";
  get_difficulty_helper ()

(** Prompts the user for a line input and returns the char char array
    represented by the .csv file specified by the input. Prompts again
    if the input does not specify a valid file. *)
let rec get_board () =
  clear ();
  print_endline
    "Please enter the name of the board file you want to load. The \
     main board is 'maze.csv'.";
  print_string "> ";
  let rec helper () =
    let str = read_line () in
    try Maze.csv_to_array str
    with Sys_error _ ->
      print_string "\nFile not found, try again.\n> ";
      helper ()
  in
  helper ()

(** The string representing the contents of a .txt file specified by
    [fname]. *)
let txt_to_string fname =
  let ch = open_in fname in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(** Runs the game. *)
let rec main () =
  welcome ();
  single_char_input ();
  let input = input_char stdin in
  revert_terminal ();
  match input with
  (* Play the game. *)
  | '1' -> play_game ()
  (* How to play screen. *)
  | '2' -> how_to_play ()
  (* How to make a board screen. *)
  | '3' -> how_to_make_board ()
  (* Highscores. *)
  | '4' -> highscores ()
  (* Quit. *)
  | '5' -> clear ()
  (* Invalid input *)
  | _ -> main ()

(** Prints a welcome message. *)
and welcome () =
  clear ();
  ANSITerminal.print_string [ ANSITerminal.red ] "Welcome to Pac-Man.\n";
  print_endline
    "What would you like to do?\n\
    \ 1. Play\n\
    \ 2. How to play\n\
    \ 3. How to make a Pac-Man board\n\
    \ 4. View Highscores\n\
    \ 5. Quit"

(* Helper for play_game(). *)
and update_state_ref state_ref cmd_ref cmd =
  (cmd_ref :=
     match cmd with
     | Some Pause | Some Quit | None -> !cmd_ref
     | Some (Move m) -> Move m);
  state_ref :=
    match cmd with
    | Some command -> State.update !state_ref command
    | None -> State.update !state_ref !cmd_ref

(** Plays the Pac-Man game. *)
and play_game () =
  let maze_array = get_board () in
  let difficulty = get_difficulty () in
  let state = ref (State.init_state maze_array difficulty) in
  let command = ref (Move Right) in
  State.print !state;
  while not (State.quit_game !state) do
    if State.game_state !state = Playing then begin
      update_state_ref state command
        (try timeout user_input () time_per_frame with Timeout -> None);
      State.print !state
    end
    else if State.game_state !state = Won then begin
      print_endline "Congrats You Won!";
      state := State.update !state Quit;
      print_endline
        "Please type your name and hit Enter to add it to the \
         leaderboard:";
      let name = read_line () in
      update_leaderboard (name ^ "\t") state difficulty
    end
    else if State.game_state !state = Lost then begin
      print_endline "You Lost :(";
      state := State.update !state Quit
    end
  done;
  print_endline
    "Thanks for playing! Press Enter to return to the main menu.";
  ignore (read_line ());
  main ()

(** Prints "how to play" screen. *)
and how_to_play () =
  clear ();
  print_endline (txt_to_string "how_to/howtoplay.txt");
  print_endline "";
  print_endline "Press Enter to return to the main menu.";
  ignore (read_line ());
  main ()

(** Prints "how to make board" screen. *)
and how_to_make_board () =
  clear ();
  print_endline (txt_to_string "how_to/howtoboard.txt");
  print_endline "";
  print_endline "Press Enter to return to the main menu.";
  ignore (read_line ());
  main ()

(** Prints highscores. *)
and highscores () =
  clear ();
  let difficulty = select_level () in
  let info =
    match difficulty with
    | Easy -> ("leaderboards/leaderboard_easy.csv", "Easy")
    | Normal -> ("leaderboards/leaderboard_normal.csv", "Normal")
    | Hard -> ("leaderboards/leaderboard_hard.csv", "Hard")
  in
  write_csv (fst info) (snd info);

  print_endline "";
  print_endline "Press Enter to return to the main menu.";
  ignore (read_line ());
  main ()

(** Updates the corresponding leaderboard based on [difficulty] with the
    player's name and score. *)
and update_leaderboard name state difficulty =
  let fname =
    match difficulty with
    | Easy -> "leaderboard_easy.csv"
    | Normal -> "leaderboard_normal.csv"
    | Hard -> "leaderboard_hard.csv"
  in
  Highscores.add_score
    ("leaderboards/" ^ fname)
    name
    (string_of_int (State.score !state))

(* Execute the game engine. *)
let () = main ()
