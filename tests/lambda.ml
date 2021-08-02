(** Some encoding in the lambda calculus *)

external print_string : string -> unit = "print"
external print_endline : string -> unit = "println"

let print_newline _ = print_endline ""

(** Church encoding of natural numbers *)

let zero f x = x
let one f x = f x
let two f x = f (f x)

let print_num num =
  num (fun _ -> print_string ".") ();
  print_newline ()

let _ =
  print_num one;
  print_num two

let succ n = fun f x -> (n f (f x))

let plus m n = m succ n

let _ =
  print_num (plus one two)

let mult m n = m (fun k -> (plus n k)) zero

let _ =
  print_num (mult (plus one two) two)

let pow b e = e b

let pred n =
  fun f x -> n (fun g h -> h (g f)) (fun _ -> x) (fun u -> u)

let _ =
  print_num (pred (mult (plus one two) two))

let sub m n = pred m

(** Encoding of booleans and logic *)

let true_ (x : 'a) (y : 'a) = x
let false_ (x : 'a) (y : 'a) = y

let print_bool b =
  b (fun _ -> print_string "true") (fun _ -> print_string "false") ();
  print_newline ()

let _ =
  print_bool true_;
  print_bool false_

let orb p q =
  p true_ q

let andb p q =
  p q false_

let notb p =
  p false_ true_

let iszero n =
  n (fun x -> false_) true_

let leq m n =
  iszero (sub m n)

let _ =
  print_bool (orb true_ false_);
  print_bool (orb false_ false_)
