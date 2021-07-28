let usage = "./mltokotlin.exe <input>"

let src = ref []

let add_src f =
  src := f::!src

(** Parse a file and return the relevant program *)
let parse file =
  let cin = open_in file in
  let p = cin |> Lexing.from_channel |> Parse.implementation in
  close_in cin;
  p

(** Type a parsed program *)
let type_prog file prog =
  let (tp, _) =
    try
      Compmisc.init_path ~dir:"/dev/null" ();
      let env = Compmisc.initial_env () in
      Typemod.type_implementation file "dummy_outputprefix" "dummy_modulename" env prog
    with
    | Typecore.Error (loc, env, err) ->
      begin
        Typecore.report_error ~loc env err |>
        Location.print_report Format.err_formatter;
        exit 1
      end
    | Env.Error e ->
      Env.report_error Format.err_formatter e;
      exit 1
  in
  Printtyped.implementation Format.std_formatter tp

(* Configure the ocaml flags *)
let _ =
  (* We dont want to generate the cmi interface (for now) *)
  Clflags.dont_write_files := true;
  (* We dont use the ocaml stdlib, but a clone with the correct externals (and less features) *)
  Clflags.no_std_include := true;
  Clflags.nopervasives := true

(* Entry point *)
let _ =
  Arg.parse [] add_src usage;

  let f = List.hd !src in
  parse f |> type_prog f
