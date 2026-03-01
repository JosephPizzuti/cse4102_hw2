(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 2 **)
(** Tail Recursion/HOFs **)

exception UnequalLengths

let vector_add (v1 : int list) (v2 : int list) : int list =
  let rec aux v1 v2 acc =
    match v1, v2 with
    | [], [] -> acc
    | (h1:t1), (h2:t2) -> aux t1 t2 (h1+h2)::acc
  in List.rev (aux v1 v2 [])

let repeat (c: char) (n: int) : char list =
  raise Util.Unimplemented

let derive (f : float -> float) : (float -> float) = 
  let epsilon = 1e-8 in
  raise Util.Unimplemented

let for_all (f : 'a -> bool) (l : 'a list) : bool =
  raise Util.Unimplemented
  
let matrix_valid (mat : int list list ) : bool = 
  raise Util.Unimplemented

let compose_all (fns : ('a -> 'a) list) : ('a -> 'a) =
  raise Util.Unimplemented

let run_length_decode (l: (char * int) list) : char list =
  raise Util.Unimplemented

let run_length_encode (l : char list) : (char * int) list =
  match l with 
  | [] -> []
  | hd::tl -> 
      let first_acc = [(hd, 1)] in 
      let final_acc = List.fold_left (fun acc next_value -> 
        raise Util.Unimplemented
      ) first_acc tl in 
      List.rev final_acc

let fold_right (f: 'a -> 'b -> 'b) (l: 'a list) (u: 'b) : 'b =
  raise Util.Unimplemented
