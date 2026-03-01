
(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 2 **)
(** N-Queens Puzzle **)

exception InvalidBoard

(*
   Board representation:
   A board is a list of column indices.
   The i-th element is the column of the queen in row i.
*)
type board = int list

(* [0; 1; ...; n-1] *)
let range (n : int) : int list =
  List.init n (fun x -> x)

(* Check whether a board is valid (no queens attacking each other) *)
let is_board_valid (b : board) : bool =
  let rec check = function
    | [] -> true
    | (r1, c1) :: rest ->
        List.for_all (fun (r2, c2) ->
          c1 <> c2 && abs (c1 - c2) <> abs (r1 - r2)
        ) rest && check rest
  in
  check (List.mapi (fun i c -> (i, c)) b)

(* Check whether a list of boards are all valid *)
let is_board_list_valid (boards : board list) : bool =
  List.for_all (fun b -> is_board_valid b) boards

(* Check whether all boards in a list are unique *)
let is_board_list_all_unique (boards : board list) : bool =
  let sorted_boards = List.sort compare boards in
  let rec has_dupes = function
    | [] | [_] -> false
    | b1 :: b2 :: _ when b1 = b2 -> true
    | _ :: rest -> has_dupes rest
  in
  not (has_dupes sorted_boards)

(* Prints a board *)
let print_board (board : board) : unit =
  List.iter (fun x -> print_int x; print_string " ") board

(* Prints a list of boards *)
let print_board_list (boards : board list) : unit=
  List.iter (fun board ->
    print_string "[ ";
    print_board board;
    print_endline "]"
  ) boards

(* Pretty-prints a board 
  For example, [1; 3; 0; 2] would be printed as:
   -Q--
   ---Q
   Q---
   --Q-
*)
let print_board_pretty (n : int) (b : board) : unit =
  let print_row c =
    range n
    |> List.map (fun j -> if j = c then 'Q' else '-')
    |> List.to_seq
    |> String.of_seq
    |> print_endline
  in
  List.iter print_row b;
  if not (is_board_valid b) then
    raise InvalidBoard
  else
    print_endline (String.make (n+2) '~')

(* Check whether placing a queen in the next row at column c is safe *)
let safe (b : board) (c : int) : bool =
  let k = List.length b in
  List.for_all (fun x -> x) 
      (List.mapi (fun ri ci -> ci <> c && abs (ci - c) <> (k - ri)) b)

(* Extend a partial board by adding one queen in a safe column *)
let extend_board (n : int) (b : board) : board list =
  range n
  |> List.filter (fun col -> safe b col)
  |> List.map (fun col -> b @ [col])

(* Solves the n-queens problem *)
let solve_nqueens (n : int) : board list =
  range n
  |> List.fold_left (fun curr_boards _ ->
      curr_boards
      |> List.map (fun board -> extend_board n board)
      |> List.flatten
  ) [[]]

(* Number of solutions *)
let count (n : int) : int =
  List.length (solve_nqueens n)

let main () =
  let arg =
    try Sys.argv.(1)
    with Invalid_argument _ -> (Printf.eprintf "Usage: nqueens <N>\n"; exit 1)
  in
  let n =
    try int_of_string arg
    with Failure _ -> (Printf.eprintf "Usage: nqueens <N>\n"; exit 1)
  in
  let sols = solve_nqueens n in
  Printf.printf "Number of solutions for n=%d: %d\n\n" n (List.length sols);
  Printf.printf "All solutions valid? %b\n\n" (is_board_list_valid sols);
  Printf.printf "All solutions unique? %b\n\n" (is_board_list_all_unique sols);
  (* try List.iter (print_board_pretty n) sols *)
  (*List.iter (print_board_pretty n) sols; *)
  let partial_board = extend_board 4 [1;3] in
  Printf.printf "Extend board solutions value? %b\n" (is_board_list_valid partial_board);
  Printf.printf "Extend board solutions unique? %b\n" (is_board_list_all_unique partial_board);

  let sol_f2 = solve_nqueens 2 in
  let sol_f3 = solve_nqueens 3 in
  Printf.printf "Number of solutions for 2: %d\n" (List.length sol_f2);
  Printf.printf "num solutions for 3: %d\n" (List.length sol_f3)

let _ = main ()
