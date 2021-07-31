(** Compiler environnement, mutable *)

let mains = ref []

let register_main s =
  mains := s::!mains

let get_mains () = List.rev !mains

module Env = Map.Make(String)

let primitives = ref Env.empty

let register_primitive path prim =
  primitives := Env.add path prim !primitives

let get_primitive_opt path =
  Env.find_opt path !primitives
