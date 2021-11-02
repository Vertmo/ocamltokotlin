(** Compiler environnement, mutable *)

(** Set initial environment *)
val set_env : Env.t -> unit

(** Main functions *)

(** Register a new function that will be called from main *)
val register_main : Ident.t -> unit

(** Get all the functions registered to be called from main *)
val get_mains : unit -> Ident.t list

(** Primitives *)

(* (\** Register a primitive *\)
 * val register_primitive : Path.t -> string -> unit *)

(** Get a primitive if it exists *)
val get_primitive_opt : Path.t -> string option

(** Typing environnement for the Kotlin program *)

(** Find a global value in the environnement *)
val find_global : Path.t -> Types.type_expr
