(** Highscores set by players. *)

(**Loads the file given by the string filename *)
val load : string -> Csv.t

(** Sorts the .csv file given by the filename into descending order
    based on score *)
val sort_board : string -> unit

(** Prints the given leaderboard in descending order of score *)
val write_csv : string -> string -> unit

(** Adds a given score, with a given name, to the given .csv file. *)
val add_score : string -> string -> string -> unit
