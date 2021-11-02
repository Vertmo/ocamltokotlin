open PrintingLib

type 'a list =
  | Nil
  | Cons of 'a * 'a list

let rec last (l : 'a list) =
  match l with
  | Cons (_ as hd1, Nil) -> hd1
  | Cons (_, tl) -> last tl

let rec last2 = function
  | Cons (_ as hd1, Nil) -> hd1
  | Cons (_, tl) -> last tl

let _ =
  print_int (last (Cons (1, Cons (2, Cons (3, Cons (4, Nil))))));
  print_int (last2 (Cons (1, Cons (2, Cons (3, Cons (4, Nil))))));
  print_int (last Nil)

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
