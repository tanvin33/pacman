(** The current state of the Pac-Man game *)

(** The abstract type of values representing the current state of the
    game. *)
type t

(** The type representing the game status as playing, won, or lost. *)
type game_over =
  | Playing
  | Won
  | Lost

(** The type representing the difficulty setting. *)
type difficulty =
  | Easy
  | Normal
  | Hard

(** Return the difficulty represented by a string. If the string does
    not represent any difficulty, raise an exception. *)
val difficulty_of_string : string -> difficulty

(** Initialize a state based on a double array of characters and a
    difficulty. *)
val init_state : char array array -> difficulty -> t

(** Initialize a state with the specified parameters: maze, Pac-Man,
    ghost list, lives, score, paused, quit, game over, exit tiles, max
    orbs, and difficulty. *)
val make_state :
  Maze.t ->
  Pacman.t ->
  Ghost.t list ->
  int ->
  int ->
  bool ->
  bool ->
  game_over ->
  Maze.pos list ->
  int ->
  difficulty ->
  t

(** Pacman of a state. *)
val pacman : t -> Pacman.t

(** Ghosts of a state. *)
val ghosts : t -> Ghost.t list

(** Maze of a state. *)
val maze : t -> Maze.t

(** Exit tiles of a state. *)
val exit_tiles : t -> Maze.pos list

(** The starting/max number of orbs of a state. *)
val max_orbs : t -> int

(** The difficulty a state. *)
val difficulty : t -> difficulty

(** The lives of a state. *)
val lives : t -> int

(** The score of a state. *)
val score : t -> int

(** Boolean representing whether a state is paused. *)
val paused : t -> bool

(** Boolean representing whether or not to quit the game. *)
val quit_game : t -> bool

(** The game_over status of a state. *)
val game_state : t -> game_over

(** Change the game_over status of a state based on the maze. *)
val change_game_state : Maze.t -> t -> t

(** Takes the current state and returns the next state based on a user
    input. *)
val update : t -> Command.t -> t

(** Returns an array representing the current state. *)
val array_of_state : t -> char array array

(** Prints the current state of the game. *)
val print : t -> unit
