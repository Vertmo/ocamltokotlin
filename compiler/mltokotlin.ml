let usage = "./mltokotlin.exe <input>"

(* Configure the ocaml flags *)
let _ =
  (* We dont use the ocaml stdlib, but a clone with the correct externals (and less features) *)
  Clflags.no_std_include := true;
  Clflags.nopervasives := true

(* let src = ref []
 *
 * let add_src f =
 *   src := f::!src *)

let with_info =
  Compile_common.with_info ~native:false ~tool_name:"ocamltokotlin" ~dump_ext:"cmo"

(** Parse, type and compile and OCaml file *)
let compile source_file =
  let backend env typed =
    Compilenv.set_env env;
    let kf = Compile.file typed in
    Print_kotlin.print_file Format.std_formatter kf
  in
  try
    with_info ~source_file ~output_prefix:(Compenv.output_prefix source_file)
    @@ fun info -> (
      let pt = Compile_common.parse_impl info in
      ignore (Compile_common.typecheck_impl info pt); (* produce cmi file TODO factorize with the other type check *)
      let (typed, _, _, env) = Typemod.type_structure (Compile_common.(info.env)) pt in
      backend env typed
    )
  with Env.Error _ | Typetexp.Error _ as exn ->
    Location.report_exception Format.err_formatter exn; exit 2

(* Entry point *)
let _ =
  Arg.parse [] compile usage;
  (* List.iter compile (List.rev !src) *)
