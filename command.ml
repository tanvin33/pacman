type move =
  | Up
  | Down
  | Left
  | Right

type t =
  | Move of move
  | Quit
  | Pause

let opposite move =
  match move with
  | Up -> Down
  | Down -> Up
  | Left -> Right
  | Right -> Left
