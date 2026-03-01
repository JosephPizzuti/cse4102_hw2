(** UConn CSE 4102 **)
(** Spring 2026 **)
(** Homework 2 **)
(** Tail Recursion/HOFs **)

exception UnequalLengths

let vector_add (v1 : int list) (v2 : int list) : int list =
  let rec aux l1 l2 acc =
    match l1, l2 with
    | [], [] -> acc
    | h1::t1, h2::t2 -> aux t1 t2 ((h1+h2)::acc)
    | _ -> raise UnequalLengths
  in 
  List.rev (aux v1 v2 [])

let repeat (c: char) (n: int) : char list =
  let rec aux i acc =
    if i <= 0 then acc
    else aux (i-1) (c::acc)
  in 
  aux n []

let derive (f : float -> float) : (float -> float) = 
  let epsilon = 1e-8 in
    (fun x -> ((f (x +. epsilon)) -. (f x)) /. epsilon)

let for_all (f : 'a -> bool) (l : 'a list) : bool =
  List.fold_left (fun acc ele -> acc && (f ele)) true l
  
let matrix_valid (mat : int list list ) : bool = 
  match mat with
  | [] -> false
  | row :: _ ->
      let length = List.length row in
      for_all (fun curr -> List.length curr = length) mat

let compose_all (fns : ('a -> 'a) list) : ('a -> 'a) =
  List.fold_right (fun f acc -> (fun x -> f (acc x))) fns (fun x -> x)

let run_length_decode (l: (char * int) list) : char list =
  List.fold_right (fun (c, n) acc -> (repeat c n) @ acc) l []

let run_length_encode (l : char list) : (char * int) list =
  match l with 
  | [] -> []
  | hd::tl -> 
      let first_acc = [(hd, 1)] in 
      let final_acc = List.fold_left (fun acc next_value -> 
        match acc with
        | (curr, count)::t ->
            if next_value = curr
            then (curr, count+1)::t
            else (next_value, 1)::(curr, count)::t
        | [] -> [(next_value, 1)]
      ) first_acc tl in 
      List.rev final_acc

let fold_right (f: 'a -> 'b -> 'b) (l: 'a list) (u: 'b) : 'b =
  raise Util.Unimplemented

let () =
  assert (vector_add [1; 2; 3] [9;7;5] = [10;9;8]);
  assert (vector_add [] [] = []);
  assert (try let _ = vector_add [1;2] [1;2;3] in false with UnequalLengths -> true);

  assert (repeat 'a' 5 = ['a';'a';'a';'a';'a']);
  assert (repeat 'a' 0 = []);
  assert (repeat 'a' (-1) = []);

  let f x = x *. x in
  assert (Float.abs (derive f 3.0 -. 6.0) < 1e-5)


