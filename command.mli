(** Representation of player inputs. *)

(** Directions that the player can move. *)
type move =
  | Up
  | Down
  | Left
  | Right

(** The type of values representing player actions. *)
type t =
  | Move of move
  | Quit
  | Pause

(** Return the opposite move. For example, [opposite Up] returns [Down]. *)
val opposite : move -> move
