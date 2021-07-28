type t = {
  pos : Maze.pos;
  dir : Command.move;
  start_pos : Maze.pos;
}

let make_pacman pos dir start_pos = { pos; dir; start_pos }

let rep = 'C'

let pos t = t.pos

let dir t = t.dir

let start_pos t = t.start_pos

let move_helper t maze direction =
  let maze_array = Maze.array_of_maze maze in
  let maze_height = Array.length maze_array in
  let maze_width = Array.length maze_array.(snd t.pos) in
  let new_pos = Maze.change_pos t.pos direction in
  if
    Maze.is_wall maze new_pos
    || Maze.is_ghost_box maze new_pos
    || Maze.is_ghost_box_exit maze new_pos
  then t
  else
    {
      t with
      pos =
        ( (fst new_pos + maze_width) mod maze_width,
          (snd new_pos + maze_height) mod maze_height );
      dir = direction;
    }

(* If the specified direction produces a legal move, then move in that
   direction. Otherwise, move in the original direction. *)
let move t maze (dir : Command.move) =
  let moved_t = move_helper t maze dir in
  if moved_t.pos = t.pos then move_helper t maze t.dir else moved_t

let reset_pos t = { t with pos = t.start_pos }
