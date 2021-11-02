(** Compiler environnement, mutable *)

(* module Env = Map.Make(String) *)

(** Main functions *)

let mains = ref []

let register_main s =
  mains := s::!mains

let get_mains () = List.rev !mains

(** We rely upon the ocaml typing environment *)

open Typedtree

let env = ref None

let set_env ev =
  env := Some ev

let get_env () =
  match !env with
  | Some env -> env
  | _ -> failwith "Initial env has not been set"

(** Primitives *)

let get_primitive_opt path =
  try (match Env.find_value path (get_env ()) with
      | { val_kind = Val_prim desc } ->
        Some desc.prim_name
      | _ -> None
    ) with Not_found -> None

(** Typing environnement for the Kotlin program *)

let find_global path =
  (Env.find_value path (get_env ())).val_type
