open ANSITerminal

let orb_val = 10

let big_orb_val = 50

let ghost_val = 200

type game_over =
  | Playing
  | Won
  | Lost

type difficulty =
  | Easy
  | Normal
  | Hard

let difficulty_of_string str =
  match str |> String.lowercase_ascii |> String.trim with
  | "e" | "easy" | "1" -> Easy
  | "n" | "normal" | "2" -> Normal
  | "h" | "hard" | "3" -> Hard
  | _ -> failwith "Invalid difficulty"

let difficulty_active_frac d =
  match d with
  | Easy -> 1. /. 4.
  | Normal -> 1. /. 6.
  | Hard -> 1. /. 8.

(* The number of frames that a ghost will scatter for based on the
   difficulty. *)
let scatter_frames d =
  match d with Easy -> 20 | Normal -> 10 | Hard -> 8

type t = {
  maze : Maze.t;
  pacman : Pacman.t;
  ghosts : Ghost.t list;
  lives : int;
  score : int;
  (* scatter = ghosts are running away from Pac-Man. *)
  paused : bool;
  quit_game : bool;
  game_state : game_over;
  (* The exits of the ghost box. *)
  exit_tiles : Maze.pos list;
  (* The total number of orbs in the maze before any are collected. *)
  max_orbs : int;
  difficulty : difficulty;
}

let pacman t = t.pacman

let ghosts t = t.ghosts

let maze t = t.maze

let lives t = t.lives

let score t = t.score

let paused t = t.paused

let exit_tiles t = t.exit_tiles

let max_orbs t = t.max_orbs

let difficulty t = t.difficulty

let init_state maze_array difficulty =
  let color_pos_list : (Ghost.color * Maze.pos) list ref = ref [] in
  let pacman = ref (Pacman.make_pacman (0, 0) Right (0, 0)) in
  let exit_tiles = ref [] in
  let maze = Maze.make_maze maze_array in
  for y = 0 to Array.length maze_array - 1 do
    for x = 0 to Array.length maze_array.(y) - 1 do
      match maze_array.(y).(x) with
      | 'R' -> color_pos_list := (Red, (x, y)) :: !color_pos_list
      | 'P' -> color_pos_list := (Pink, (x, y)) :: !color_pos_list
      | 'O' -> color_pos_list := (Orange, (x, y)) :: !color_pos_list
      | 'B' -> color_pos_list := (Cyan, (x, y)) :: !color_pos_list
      | 'C' -> pacman := Pacman.make_pacman (x, y) Right (x, y)
      | '=' -> exit_tiles := (x, y) :: !exit_tiles
      | _ -> ()
    done
  done;
  {
    maze;
    pacman = !pacman;
    ghosts = Ghost.make_ghosts !color_pos_list maze;
    lives = 3;
    score = 0;
    paused = false;
    quit_game = false;
    game_state = Playing;
    exit_tiles = !exit_tiles;
    max_orbs = Maze.orbs_remaining maze;
    difficulty;
  }

let update_pacman move t =
  { t with pacman = Pacman.move t.pacman t.maze move }

let update_ghost_pos t =
  {
    t with
    ghosts =
      List.map
        (Ghost.move t.pacman t.maze t.ghosts t.exit_tiles)
        t.ghosts;
  }

let rec update_scatter_frames_helper ghosts =
  match ghosts with
  | h :: t ->
      if Ghost.scatter h then
        Ghost.incr_scatter_frames h :: update_scatter_frames_helper t
      else h :: update_scatter_frames_helper t
  | [] -> []

let update_scatter_frames t =
  { t with ghosts = update_scatter_frames_helper t.ghosts }

let lose_life t =
  {
    t with
    lives = t.lives - 1;
    ghosts =
      List.map Ghost.make_not_scatter
        (List.map Ghost.reset_pos t.ghosts);
    pacman = Pacman.reset_pos t.pacman;
  }

let swapped_pos pacman ghost =
  if
    Pacman.dir pacman = Command.opposite (Ghost.dir ghost)
    && Pacman.pos pacman
       = Maze.change_pos (Ghost.pos ghost) (Pacman.dir pacman)
  then true
  else false

let rec collides pacman ghosts =
  match ghosts with
  | h :: t ->
      if
        (not (Ghost.scatter h))
        && (Ghost.pos h = Pacman.pos pacman || swapped_pos pacman h)
      then true
      else collides pacman t
  | [] -> false

(* Helper for update_collision. *)
let rec new_ghosts pacman ghosts =
  match ghosts with
  | h :: t ->
      if
        Ghost.scatter h
        && (Ghost.pos h = Pacman.pos pacman || swapped_pos pacman h)
      then
        (h |> Ghost.reset_pos |> Ghost.make_not_scatter)
        :: new_ghosts pacman t
      else h :: new_ghosts pacman t
  | [] -> []

(* Helper for update_collision. *)
let rec new_score pacman ghosts score =
  match ghosts with
  | h :: t ->
      if
        Ghost.scatter h
        && (Ghost.pos h = Pacman.pos pacman || swapped_pos pacman h)
      then new_score pacman t (score + ghost_val)
      else new_score pacman t score
  | [] -> score

let update_collision t =
  if collides t.pacman t.ghosts then lose_life t
  else
    {
      t with
      ghosts = new_ghosts t.pacman t.ghosts;
      score = new_score t.pacman t.ghosts t.score;
    }

let collect_big_orb t =
  {
    t with
    ghosts =
      List.map Ghost.make_scatter (List.map Ghost.reverse_dir t.ghosts);
    score = t.score + big_orb_val;
    maze = Maze.remove_orb (Pacman.pos t.pacman) t.maze;
  }

let collect_orb t =
  {
    t with
    score = t.score + orb_val;
    maze = Maze.remove_orb (Pacman.pos t.pacman) t.maze;
  }

let update_orbs t =
  if Maze.has_big_orb (Pacman.pos t.pacman) t.maze then
    collect_big_orb t
  else if Maze.has_orb (Pacman.pos t.pacman) t.maze then collect_orb t
  else t

let update_active_helper t ghost =
  let orbs_remaining = float_of_int (Maze.orbs_remaining t.maze) in
  let max_orbs = float_of_int t.max_orbs in
  match Ghost.color ghost with
  | Red -> Ghost.make_active ghost
  | Cyan ->
      if
        orbs_remaining
        <= max_orbs *. (1. -. difficulty_active_frac t.difficulty)
      then Ghost.make_active ghost
      else ghost
  | Pink ->
      if
        orbs_remaining
        <= max_orbs
           *. (1. -. (2. *. difficulty_active_frac t.difficulty))
      then Ghost.make_active ghost
      else ghost
  | Orange ->
      if
        orbs_remaining
        <= max_orbs
           *. (1. -. (3. *. difficulty_active_frac t.difficulty))
      then Ghost.make_active ghost
      else ghost

let update_active t =
  { t with ghosts = List.map (update_active_helper t) t.ghosts }

let rec update_ghost_scatter_helper ghosts difficulty =
  match ghosts with
  | h :: t ->
      if Ghost.scatter_frames h >= scatter_frames difficulty then
        Ghost.make_not_scatter h
        :: update_ghost_scatter_helper t difficulty
      else h :: update_ghost_scatter_helper t difficulty
  | [] -> []

let update_ghost_scatter t =
  { t with ghosts = update_ghost_scatter_helper t.ghosts t.difficulty }

let update_game_state t =
  if Maze.has_won t.maze then { t with game_state = Won }
  else if t.lives = 0 then { t with game_state = Lost }
  else t

let update t (command : Command.t) =
  if not t.paused then
    match command with
    | Move move ->
        t |> update_pacman move |> update_ghost_pos
        |> update_scatter_frames |> update_collision |> update_orbs
        |> update_active |> update_ghost_scatter |> update_game_state
    | Pause -> { t with paused = true }
    | Quit -> { t with quit_game = true }
  else
    match command with
    | Pause -> { t with paused = false }
    | Quit -> { t with quit_game = true }
    | _ -> t

let quit_game t = t.quit_game

let game_state t = t.game_state

let change_game_state maze t =
  if Maze.has_won maze then { t with game_state = Won }
  else if t.lives = 0 then { t with game_state = Lost }
  else t

let make_state
    maze
    pacman
    ghosts
    lives
    score
    paused
    quit_game
    game_state
    exit_tiles
    max_orbs
    difficulty =
  {
    maze;
    pacman;
    ghosts;
    lives;
    score;
    paused;
    quit_game;
    game_state;
    exit_tiles;
    max_orbs;
    difficulty;
  }

(* White is the orange ghost cause we ran out of colors. *)
let print_char x =
  match x with
  | 'C' ->
      print_string
        [ Bold; ANSITerminal.yellow ]
        (Char.escaped Pacman.rep)
  | 'R' ->
      print_string [ Bold; ANSITerminal.red ] (Char.escaped Ghost.rep)
  | 'O' ->
      print_string [ Bold; ANSITerminal.white ] (Char.escaped Ghost.rep)
  | 'P' ->
      print_string
        [ Bold; ANSITerminal.magenta ]
        (Char.escaped Ghost.rep)
  | 'B' ->
      print_string [ Bold; ANSITerminal.cyan ] (Char.escaped Ghost.rep)
  | 'S' ->
      print_string [ Bold; ANSITerminal.blue ] (Char.escaped Ghost.rep)
  | '#' -> print_string [ ANSITerminal.green ] (Char.escaped Maze.wall)
  | 'X' -> print_string [] (Char.escaped ' ')
  | _ -> print_string [] (Char.escaped x)

let print_row row = Array.iter print_char row

let print_dbl_array dbl_array =
  for i = 0 to Array.length dbl_array - 1 do
    print_row dbl_array.(i);
    print_endline ""
  done

let change_e_at_pos e dblarr pos =
  match pos with x, y -> dblarr.(y).(x) <- e

let char_of_ghost ghost =
  if Ghost.scatter ghost then 'S'
  else
    match Ghost.color ghost with
    | Red -> 'R'
    | Cyan -> 'B'
    | Orange -> 'O'
    | Pink -> 'P'

let array_of_state t =
  let frame = Maze.array_of_maze t.maze in
  change_e_at_pos Pacman.rep frame (Pacman.pos t.pacman);
  let rec change_ghosts frame ghosts =
    match ghosts with
    | h :: t ->
        change_e_at_pos (char_of_ghost h) frame (Ghost.pos h);
        change_ghosts frame t
    | [] -> frame
  in
  change_ghosts frame t.ghosts

let clear () = ignore (Sys.command "clear")

let print t =
  clear ();
  print_endline
    "Welcome to Pac-Man. To play, use the WASD keys to move the the \
     character around the gameboard. To pause the game press p, or \
     press q to quit. Remember to collect all the orbs and run away \
     from the ghosts!";
  print_endline ("Score: " ^ string_of_int t.score);
  print_endline ("Lives Remaining: " ^ string_of_int t.lives);
  print_dbl_array (array_of_state t);
  if t.paused then print_endline "PAUSED: click p again to resume"
