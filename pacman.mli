(** Representation of Pac-Man. *)

(** The abstract type of values representing Pac-Man. *)
type t

(** Initialize a PacMan with the specified position and direction. *)
val make_pacman : Maze.pos -> Command.move -> Maze.pos -> t

(** The character representation of Pac-Man. *)
val rep : char

(** The current position of Pac-Man. *)
val pos : t -> Maze.pos

(** The current direction of Pac-Man. *)
val dir : t -> Command.move

(** The starting position of Pac-Man. *)
val start_pos : t -> Maze.pos

(** Send Pac-Man back to its starting position. *)
val reset_pos : t -> t

(** Changes the position of Pac-Man by 1 in either the horizontal or
    vertical directions if it is a legal move (e.g. not through a wall). *)
val move : t -> Maze.t -> Command.move -> t
