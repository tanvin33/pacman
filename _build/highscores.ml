let load file = Csv.load file

let sort_helper a b =
  let score1 = List.nth a 1 in
  let score2 = List.nth b 1 in
  if int_of_string score1 > int_of_string score2 then -1 else 1

let sort_board fname =
  let board = Csv.trim (load fname) in
  Csv.save fname (List.sort sort_helper board)

let write_csv_helper lst = Csv.print_readable lst

let write_csv fname level =
  ignore (Sys.command "clear");
  print_endline "\t Pacman Leaderboard -- Top Scores:";
  print_endline ("\t \tLevel: " ^ level);
  print_endline "  ";
  print_endline "NAME       SCORE";
  print_endline "---------------------------------------";
  sort_board fname;
  let entries = load fname in
  write_csv_helper entries

let format_name name =
  let max_name_size = 10 in
  let name_length = String.length name in
  if name_length >= 10 then String.sub name 0 10 else name

let add_score fname name score =
  let entries = load fname in
  let new_entry = [ format_name name; score ] in
  let new_file = entries @ [ new_entry ] in
  Csv.save fname new_file
