type color =
  | Cyan
  | Red
  | Pink
  | Orange

type t = {
  pos : Maze.pos;
  dir : Command.move;
  color : color;
  active : bool;
  scatter : bool;
  scatter_frames : int;
  start_pos : Maze.pos;
}

let rep = 'G'

let color t = t.color

let pos t = t.pos

let dir t = t.dir

let active t = t.active

let reverse_dir t =
  {
    t with
    dir =
      (match t.dir with
      | Left -> Right
      | Right -> Left
      | Up -> Down
      | Down -> Up);
  }

let make_active t = { t with active = true }

let make_scatter t = { t with scatter = true; scatter_frames = 0 }

let make_not_scatter t = { t with scatter = false; scatter_frames = 0 }

(* Distance from pos1 to pos2 in a straight line. *)
let distance (pos1 : Maze.pos) (pos2 : Maze.pos) =
  let v = (fst pos2 - fst pos1, snd pos2 - snd pos1) in
  sqrt
    (Float.pow (v |> fst |> float_of_int) 2.
    +. Float.pow (v |> snd |> float_of_int) 2.)

let scatter_tile maze color =
  let array_of_maze = Maze.array_of_maze maze in
  match color with
  | Red -> (Array.length array_of_maze.(1) - 1, 0)
  | Pink -> (0, 0)
  | Cyan ->
      ( Array.length array_of_maze.(1) - 1,
        Array.length array_of_maze - 1 )
  | Orange -> (0, Array.length array_of_maze - 1)

(* Change [pos] by [dist] tiles in direction [dir]. Requires [dist] is
   >= 0. *)
let rec change_pos pos dir dist =
  if dist = 0 then pos
  else change_pos (Maze.change_pos pos dir) dir (dist - 1)

let red_target_pos pacman = Pacman.pos pacman

let cyan_target_pos red_ghost pacman =
  let pacman_pos = Pacman.pos pacman in
  let pacman_dir = Pacman.dir pacman in
  let red_pos = pos red_ghost in
  let two_in_front = change_pos pacman_pos pacman_dir 2 in
  let v =
    (fst two_in_front - fst red_pos, snd two_in_front - snd red_pos)
  in
  (fst two_in_front + fst v, snd two_in_front + snd v)

let pink_target_pos pacman =
  let pacman_pos = Pacman.pos pacman in
  let pacman_dir = Pacman.dir pacman in
  change_pos pacman_pos pacman_dir 4

let orange_target_pos t pacman maze =
  let pacman_pos = Pacman.pos pacman in
  if distance pacman_pos t.pos <= 8. then scatter_tile maze t.color
  else pacman_pos

let red_ghost tlist =
  let rec red_ghost_helper lst =
    match lst with
    | h :: tail -> if color h = Red then h else red_ghost_helper tail
    | [] -> raise (Failure "Red ghost is missing.")
  in
  red_ghost_helper tlist

let target_pos t pacman ghosts maze =
  match t.scatter with
  | false -> (
      match t.color with
      | Red -> red_target_pos pacman
      | Cyan -> cyan_target_pos (red_ghost ghosts) pacman
      | Orange -> orange_target_pos t pacman maze
      | Pink -> pink_target_pos pacman)
  | true -> scatter_tile maze t.color

let change_pos_helper dx dy pos =
  match pos with x, y -> (x + dx, y + dy)

let change_pos pos (dir : Command.move) =
  match dir with
  | Down -> change_pos_helper 0 1 pos
  | Up -> change_pos_helper 0 (-1) pos
  | Left -> change_pos_helper (-1) 0 pos
  | Right -> change_pos_helper 1 0 pos

(* Resets the environment before creating a new random integer within
   the bound. *)
let random_int bound =
  Random.self_init ();
  Random.int bound

(* Compare the distances, but if they are the same, randomize their
   order. *)
let compare pair1 pair2 =
  let dist1 = snd pair1 in
  let dist2 = snd pair2 in
  if dist1 > dist2 then 1
  else if dist1 < dist2 then -1
  else
    let index = random_int 2 in
    let array = [| 1; -1 |] in
    array.(index)

(* Return a list of (direction, distance) pairs sorted in ascending
   order of distance. *)
let dir_dist_lst t target_pos =
  let ghost_pos = t.pos in
  let dir_lst : Command.move list = [ Up; Down; Left; Right ] in
  let pos_lst = List.map (change_pos ghost_pos) dir_lst in
  let dist_lst = List.map (distance target_pos) pos_lst in
  let pairs_lst = List.combine dir_lst dist_lst in
  List.sort compare pairs_lst

let move_helper t maze direction blocked =
  let maze_array = Maze.array_of_maze maze in
  let maze_height = Array.length maze_array in
  let maze_width = Array.length maze_array.(snd t.pos) in
  let new_pos = Maze.change_pos t.pos direction in
  if blocked new_pos then t
  else
    {
      t with
      pos =
        ( (fst new_pos + maze_width) mod maze_width,
          (snd new_pos + maze_height) mod maze_height );
      dir = direction;
    }

(* Move a direction to the end of a list of (direction, distance) pairs. *)
let rec move_to_end (direction : Command.move) pairslst =
  match pairslst with
  | (dir, dist) :: t ->
      if dir = direction then t @ [ (dir, dist) ]
      else (dir, dist) :: move_to_end direction t
  | [] -> []

(* Try to move in all the directions in [lst] until a valid move is
   found. A valid move is a move that changes the position of the ghost
   without going through a wall. Otherwise, raise an error. *)
let rec try_moves lst maze t blocked =
  match lst with
  | (dir, dist) :: tail ->
      let moved_t = move_helper t maze dir blocked in
      if moved_t.pos = t.pos then try_moves tail maze t blocked
      else moved_t
  | [] -> t

let blocked_move_normal maze pos =
  Maze.is_wall maze pos
  || Maze.is_ghost_box_exit maze pos
  || Maze.is_ghost_box maze pos

let move_normal pacman maze ghosts t =
  (* The list of directions sorted in order of minimizing the distance
     to the target position. *)
  let sorted_dir = dir_dist_lst t (target_pos t pacman ghosts maze) in
  (* Move the option of going backwards to the end of the list so that
     it is only tried when the Ghost has no other options. *)
  let sorted_valid_dir =
    match t.dir with
    | Up -> move_to_end Down sorted_dir
    | Down -> move_to_end Up sorted_dir
    | Left -> move_to_end Right sorted_dir
    | Right -> move_to_end Left sorted_dir
  in
  try_moves sorted_valid_dir maze t (blocked_move_normal maze)

let blocked_move_ghost_box maze pos = Maze.is_wall maze pos

(* Take the direction that minimizes the distance to any exit. *)
let move_ghost_box pacman maze ghosts t exit_tiles =
  let dir_list = List.flatten (List.map (dir_dist_lst t) exit_tiles) in
  let sorted_dir_list = List.sort compare dir_list in
  try_moves sorted_dir_list maze t (blocked_move_ghost_box maze)

let blocked_move_exit_tile maze pos =
  Maze.is_wall maze pos || Maze.is_ghost_box maze pos

let move_exit_tile pacman maze ghosts t =
  let dir_list = dir_dist_lst t (0, 0) in
  try_moves dir_list maze t (blocked_move_exit_tile maze)

let move_active pacman maze ghosts t exit_tiles =
  if Maze.is_ghost_box maze t.pos then
    move_ghost_box pacman maze ghosts t exit_tiles
  else if Maze.is_ghost_box_exit maze t.pos then
    move_exit_tile pacman maze ghosts t
  else move_normal pacman maze ghosts t

let randomized_dir_lst () =
  let dir_lst : (Command.move * int) list =
    [
      (Up, random_int 100);
      (Down, random_int 100);
      (Left, random_int 100);
      (Right, random_int 100);
    ]
  in
  List.sort compare dir_lst

let blocked_inactive maze pos = not (Maze.is_ghost_box maze pos)

(* Do not let the ghost leave the ghost box and move randomly inside the
   ghost box. If the ghost is already out of the ghost box, then make it
   active. *)
let move_inactive pacman maze ghosts t exit_tiles =
  if not (Maze.is_ghost_box maze t.pos) then
    move_active pacman maze ghosts { t with active = true } exit_tiles
  else
    let dir_list = randomized_dir_lst () in
    try_moves dir_list maze t (blocked_inactive maze)

(* If the specified direction produces a legal move, then move in that
   direction. Otherwise, try the next best move. Rule: The ghost cannot
   move backwards unless it hits a dead-end. *)
let move pacman maze ghosts exit_tiles t =
  match t.active with
  | true -> move_active pacman maze ghosts t exit_tiles
  | false -> move_inactive pacman maze ghosts t exit_tiles

let init_ghost_dir t maze =
  (* The ghosts have an equal chance of starting in any direction. *)
  let rec try_moves_dir lst =
    match lst with
    | (dir, num) :: tail ->
        let moved_t =
          move_helper t maze dir (blocked_move_ghost_box maze)
        in
        if moved_t.pos = t.pos then try_moves_dir tail else dir
    | [] -> Up
  in
  try_moves_dir (randomized_dir_lst ())

let make_ghost pos dir color active scatter scatter_frames start_pos =
  { pos; dir; color; active; scatter; scatter_frames; start_pos }

let rec make_ghosts (color_pos_list : (color * Maze.pos) list) maze =
  match color_pos_list with
  | (color, pos) :: t ->
      {
        color;
        pos;
        dir =
          init_ghost_dir
            {
              color;
              pos;
              dir = Up;
              active = false;
              scatter = false;
              scatter_frames = 0;
              start_pos = pos;
            }
            maze;
        active = false;
        scatter = false;
        scatter_frames = 0;
        start_pos = pos;
      }
      :: make_ghosts t maze
  | [] -> []

let scatter_frames t = t.scatter_frames

let incr_scatter_frames t =
  { t with scatter_frames = t.scatter_frames + 1 }

let scatter t = t.scatter

let start_pos t = t.start_pos

let reset_pos t = { t with pos = t.start_pos }
