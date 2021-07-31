(** Compiler environnement, mutable *)

(** Register a new function that will be called from main *)
val register_main : string -> unit

(** Get all the functions registered to be called from main *)
val get_mains : unit -> string list

(** Register a new primitive *)
val register_primitive : string -> string -> unit

(** Get a primitive if it exists *)
val get_primitive_opt : string -> string option
