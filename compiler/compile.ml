(** Compiler functions *)

open Utils
open Typedtree
open Kotlin

let to_literal = function
  | Asttypes.Const_int i -> IntegerLit i
  | Asttypes.Const_string (s, _, _) -> StringLit s

let constructor constr params =
  match Types.(constr.cstr_name) with
  | "()" -> None
  | "true" -> Some (Literal (BooleanLit true))
  | "false" -> Some (Literal (BooleanLit false))

let rec longident (id : Longident.t) =
  (* TODO some import maybe ? *)
  let id = Longident.last id in
  Option.value (Compilenv.get_primitive_opt id) ~default:id

let rec expression (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_constant c -> [], Some (Literal (to_literal c))
  | Texp_construct (_, constr, params) -> [], constructor constr params
  | Texp_apply (e, args) ->
    let (stmts1, e') = expression e in
    let (stmts2, args') = function_args args in
    stmts2, Some (FunCall (Option.get e', args'))
  | Texp_ident (_, ident, _) -> [], Some (Ident (longident ident.txt))

and function_args = function
  | [] -> [], []
  | (_, None)::tl -> function_args tl
  | (_, Some e)::tl ->
    let stmts1, e' = expression e and stmts2, es' = function_args tl
    in stmts1@stmts2, Option.fold ~none:es' ~some:(fun e' -> e'::es') e'

let expression_as_main (expr : Typedtree.expression) =
  let fid = Atom.fresh "main" in
  Compilenv.register_main fid;
  let (stmts, e) = expression expr in
  FunctionDecl (fid, [], stmts@(Option.fold ~none:[] ~some:(fun x -> [Expression x]) e))

(** Convert a value binding to a declaration *)
let value_binding (bind : value_binding) =
  match bind.vb_pat.pat_desc with
  | Tpat_any -> expression_as_main bind.vb_expr

(* Env *)

(** Convert a structure item to one or several declarations *)
let structure_item (st : Typedtree.structure_item) =
  match st.str_desc with
  | Tstr_value (_, vals) -> List.map value_binding vals
  | Tstr_eval (e, _) -> [expression_as_main e]
  | Tstr_primitive ({ val_prim = [prim] } as desc) ->
    Compilenv.register_primitive desc.val_name.txt prim; []

let make_main () =
  let mains = Compilenv.get_mains () in
  FunctionDecl ("main", [], List.map (fun f -> Expression (FunCall (Ident f, []))) mains)

let file (st : structure) =
  let decls = List.concat_map structure_item st.str_items in
  { package_header = "mypackage";
    imports = [];
    declarations = decls@[make_main ()]; }
