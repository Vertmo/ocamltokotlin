(** Compiler environnement, mutable *)

(** Main functions *)

(** Register a new function that will be called from main *)
val register_main : string -> unit

(** Get all the functions registered to be called from main *)
val get_mains : unit -> string list

(** Primitives *)

(** Register a new primitive *)
val register_primitive : string -> string -> unit

(** Get a primitive if it exists *)
val get_primitive_opt : string -> string option

(** Global typing environnement for the Kotlin program *)

(** Register a type in the global environnement *)
val register_global : string -> Kotlin.type_var list * Kotlin.kotlin_type -> unit

(** Find a type in the global environnement *)
val find_global : string -> Kotlin.type_var list * Kotlin.kotlin_type

(* (\** Local typing environnement for the Kotlin program *\)
 * type local_env
 *
 * (\** Find a type in the local typing environnement *\)
 * val find_local_type : string -> local_env -> Kotlin.kotlin_type
 *
 * (\** Add a type in the local typing environnement *\)
 * val add_local_type : string -> Kotlin.kotlin_type -> local_env -> local_env *)
