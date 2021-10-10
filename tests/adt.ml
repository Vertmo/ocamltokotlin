external print_int : int -> unit = "print"

type int_list =
  | Nil
  | Cons of int * int_list

let rec last (l : int_list) =
  match l with
  | Nil -> 0
  | Cons (_ as hd1, Nil) -> hd1
  | Cons (_, tl) -> last tl

(** Partial match **)
let rec last_part (l : int_list) =
  match l with
  | Cons (_ as hd1, Nil) -> hd1
  | Cons (_, tl) -> last tl

let _ =
  print_int (last (Cons (1, Cons (2, Cons (3, Cons (4, Nil))))))(* ;
   * print_int (last_part Nil) *)

(** Matching a constant **)
let match_const x =
  match x with
  | 42 -> true
  | _ -> false

type t1 = A | B of t2 * t2
and t2 = C | D of t1

(** Or-matching *)
let match_or x =
  match x with
  | A | B ((C | D _), (D _ | C)) -> true
