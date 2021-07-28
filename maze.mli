(** Representation of static maze data. *)

(** The abstract type of values representing mazes. Contains information
    on the location of walls and orbs. *)
type t

(** The position of an element in the maze. The element in the top left
    is (0, 0). The y-value increases as we move downwards and the
    x-value increases as we move towards the right.*)
type pos = int * int

(** The character that represents a wall in the maze. *)
val wall : char

(** Initialize the maze represented by the nested array input. The
    nested array input should contain only '#', ' ', and 'o' *)
val make_maze : char array array -> t

(** Returns an array representation of the maze. *)
val array_of_maze : t -> char array array

(** Checks whether the element at position [pos] is a wall. Returns
    false if the given [pos] is out of bounds. *)
val is_wall : t -> pos -> bool

(** Checks whether the element at position [pos] is a ghost box tile.
    Returns false if the given [pos] is out of bounds. *)
val is_ghost_box : t -> pos -> bool

(** Checks whether the element at position [pos] is an exit tile.
    Returns false if the given [pos] is out of bounds. *)
val is_ghost_box_exit : t -> pos -> bool

(** Changes the position by 1 in either the horizontal or vertical
    directions. *)
val change_pos : pos -> Command.move -> pos

(** The number of orbs remaining in the maze. *)
val orbs_remaining : t -> int

(** Removes the orb at position [pos]. *)
val remove_orb : pos -> t -> t

(** Returns true if there are no orbs left, else false *)
val has_won : t -> bool

(** Returns true if the position [pos] in maze [t] has a big orb. *)
val has_big_orb : pos -> t -> bool

(** Returns true if the position [pos] in maze [t] has a small orb. *)
val has_orb : pos -> t -> bool

(** Takes the .csv file specified by [string] and returns the char array
    array that it represents. Raises an error if it is an invalid file
    name. *)
val csv_to_array : string -> char array array
