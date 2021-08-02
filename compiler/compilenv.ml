(** Compiler environnement, mutable *)

module Env = Map.Make(String)

(** Main functions *)

let mains = ref []

let register_main s =
  mains := s::!mains

let get_mains () = List.rev !mains

(** Primitives *)

let primitives = ref Env.empty

let register_primitive path prim =
  primitives := Env.add path prim !primitives

let get_primitive_opt path =
  Env.find_opt path !primitives

(** Global typing environnement for the Kotlin program *)

type global_env = (Kotlin.type_var list * Kotlin.kotlin_type) Env.t

let global_env = ref Env.empty

let register_global x ty =
  global_env := Env.add x ty !global_env

let find_global x =
  Env.find x !global_env

(** Local typing environnement for the Kotlin program *)

type local_env = Kotlin.kotlin_type Env.t

let find_type = Env.find
