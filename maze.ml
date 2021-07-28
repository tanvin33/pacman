type t = char array array

type pos = int * int

let wall = '#'

let orb = '.'

let ghost_box = 'X'

let ghost_box_exit = '='

let bigorb = '*'

let empty = ' '

let array_of_maze t = Array.map Array.copy t

let is_ghost_box t pos =
  try t.(snd pos).(fst pos) = ghost_box
  with Invalid_argument e -> false

let is_ghost_box_exit t pos =
  try t.(snd pos).(fst pos) = ghost_box_exit
  with Invalid_argument e -> false

let is_wall t pos =
  try t.(snd pos).(fst pos) = wall with Invalid_argument e -> false

(* Helper for orbs_remaining_helper *)
let rec orbs_in_row row =
  match row with
  | h :: t ->
      if h = orb || h = bigorb then 1 + orbs_in_row t else orbs_in_row t
  | [] -> 0

let rec orbs_remaining_helper tlist =
  match tlist with
  | row :: rows -> orbs_in_row row + orbs_remaining_helper rows
  | [] -> 0

let orbs_remaining t =
  let tlist = Array.to_list (Array.map Array.to_list t) in
  orbs_remaining_helper tlist

let fill e =
  match e with
  | 'B' | 'R' | 'O' | 'P' -> 'X'
  | 'C' -> ' '
  | ' ' -> orb
  | _ -> e

let fill_row row = Array.map fill row

let make_maze mazeArray = Array.map fill_row mazeArray

let remove_orb pos t =
  let copy = Array.map Array.copy t in
  let e = copy.(snd pos).(fst pos) in
  if e = orb || e = bigorb then (
    copy.(snd pos).(fst pos) <- empty;
    copy)
  else copy

let change_pos_helper dx dy pos =
  match pos with x, y -> (x + dx, y + dy)

let change_pos pos (dir : Command.move) =
  match dir with
  | Down -> change_pos_helper 0 1 pos
  | Up -> change_pos_helper 0 (-1) pos
  | Left -> change_pos_helper (-1) 0 pos
  | Right -> change_pos_helper 1 0 pos

let has_won t = if orbs_remaining t = 0 then true else false

let has_big_orb pos t = match pos with x, y -> t.(y).(x) = bigorb

let has_orb pos t = match pos with x, y -> t.(y).(x) = orb

let csv_to_array file =
  let arr = Csv.to_array (Csv.load file) in
  Array.map (Array.map (fun s -> if s = "" then ' ' else s.[0])) arr
