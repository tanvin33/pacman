(** Representation of ghosts. *)

(** The abstract type of values representing ghosts. *)
type t

(** The type representing colors of ghosts. *)
type color =
  | Cyan
  | Red
  | Pink
  | Orange

(** The character representation of a ghost. *)
val rep : char

(** Return the color of a ghost. *)
val color : t -> color

(** Return the current position of a ghost. *)
val pos : t -> Maze.pos

(** Return the boolean representing whether or not a ghost is active. *)
val active : t -> bool

(** Return the boolean representing whether or not the ghost is
    scattering. *)
val scatter : t -> bool

(** Return the direction that the ghost is moving. *)
val dir : t -> Command.move

(** Return the position that a ghost should be aiming to move to
    depending on their color, the other ghosts, Pac-Man, and the maze.*)
val target_pos : t -> Pacman.t -> t list -> Maze.t -> Maze.pos

(** Moves the ghost to its next position based on Pac-Man's position,
    the other ghosts' positions, the maze, and the exit tiles
    [Maze.pos list]. *)
val move : Pacman.t -> Maze.t -> t list -> Maze.pos list -> t -> t

(** Reverse the direction of a ghost. *)
val reverse_dir : t -> t

(** Make a ghost scatter. *)
val make_scatter : t -> t

(** Make a ghost not scatter. *)
val make_not_scatter : t -> t

(** Make a ghost active. *)
val make_active : t -> t

(** Initialize a ghost with the specified position, direction, color,
    scatter, active, scatter frames, and starting position. *)
val make_ghost :
  Maze.pos ->
  Command.move ->
  color ->
  bool ->
  bool ->
  int ->
  Maze.pos ->
  t

(** The number of frames a ghost has been in scatter for. *)
val scatter_frames : t -> int

(** Increment the scatter frames of a ghost by 1. *)
val incr_scatter_frames : t -> t

(** Initialize a list of ghosts given a list of pairs of colors and
    their starting positions. *)
val make_ghosts : (color * Maze.pos) list -> Maze.t -> t list

(** The starting position of a ghost. *)
val start_pos : t -> Maze.pos

(** Send a ghost back to its starting position. *)
val reset_pos : t -> t
