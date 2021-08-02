(** Compiler functions *)

open Utils
open Typedtree
open Kotlin

let todo func case = failwith (Printf.sprintf "TODO: %s : %s" func case)

(** Search for the type of a variable, first in the local environnement,
 ** then in the global *)
(* let find_type (x : string) (env : Compilenv.local_env) =
 *   try Compilenv.find_local_type x env
 *   with Not_found -> Compilenv.find_global_type x *)

let to_literal = function
  | Asttypes.Const_int i -> IntegerLit i
  | Asttypes.Const_string (s, _, _) -> StringLit s

let constructor constr params =
  match Types.(constr.cstr_name) with
  | "()" -> Literal Unit
  | "true" -> Literal (BooleanLit true)
  | "false" -> Literal (BooleanLit false)

let rec longident (id : Longident.t) =
  (* TODO some import maybe ? *)
  let id = Longident.last id in
  Option.value (Compilenv.get_primitive_opt id) ~default:id

let rec pattern (pat : 'a general_pattern) =
  match pat.pat_desc with
  | Tpat_any -> Atom.fresh "ign", []
  | Tpat_var (_, x) -> x.txt, []
  | Tpat_alias ({ pat_desc = Tpat_any }, _, alias) -> alias.txt, []
  | Tpat_alias _ -> todo "pattern" "alias"
  | Tpat_constant _ -> todo "pattern" "constant"
  | Tpat_tuple _ -> todo "pattern" "tuple"
  | Tpat_construct _ -> todo "pattern" "construct"
  | Tpat_variant _ -> todo "pattern" "variant"
  | Tpat_record _ -> todo "pattern" "record"
  | Tpat_array _ -> todo "pattern" "array"
  | Tpat_lazy _ -> todo "pattern" "lazy"
  | Tpat_or _ -> todo "pattern" "or"
    (* TODO handle destructuring functions + partial patterns *)

let rec to_kotlin_type (ty : Types.type_expr) =
  match ty.desc with
  | Tvar _ -> TypeVar ty.id
  | Tarrow (_, tin, tout, _) ->
    FunctionType ([to_kotlin_type tin], to_kotlin_type tout)
  | Tconstr (path, [], _) when Path.name path = "unit" -> TypeUnit
  | Tconstr (path, [], _) when Path.name path = "string" -> TypeString
  | Tconstr (path, [], _) when Path.name path = "int" -> TypeInt
  | Tlink ty -> to_kotlin_type ty
  | _ -> Printtyp.raw_type_expr Format.std_formatter ty; failwith "TODO"

let rec make_apply e args =
  match args with
  | [] -> e
  | hd::tl -> make_apply (FunCall (e, [hd])) tl

(** eta-expand [e] if necessary, that is if it is a global ident
 ** with a functional type *)
let eta_expand e =
  match e with
  | Ident (x, FunctionType ([tx], _))
  | TypeArguments (Ident (x, FunctionType ([tx], _)), _) ->
    (try
       let _ = Compilenv.find_global x in
       let tmp = Atom.fresh "tmp" in
       Lambda ([(tmp, tx)], [Expression (FunCall (e, [Ident (tmp, tx)]))])
     with Not_found -> e)
  | _ -> e

let rec expression (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_constant c ->
    let lit = to_literal c in
    [], Literal lit
  | Texp_construct (_, constr, params) -> [], constructor constr params
  | Texp_apply (e, args) ->
    let (stmts1, e') = expression_as_call_head e in
    let (stmts2, args') = function_args args in
    stmts2, make_apply e' args'
    (* stmts2, FunCall (e', args') *)
  | Texp_ident (_, ident, _) ->
    let stmts, e = var (longident ident.txt) expr.exp_type in
    stmts, eta_expand e
  | Texp_sequence (e1, e2) ->
    let stmts1, e1' = expression e1 and stmts2, e2' = expression e2 in
    stmts1@[Expression e1']@stmts2, e2'
  | Texp_function { cases = [{ c_lhs = pat; c_guard = None; c_rhs = e } ] } ->
    let tx = to_kotlin_type pat.pat_type in
    let (x, destr) = pattern pat in
    let (stmts1, e') = expression e in
    [], Lambda ([(x, tx)], destr@stmts1@[Expression e'])
    (* TODO *)
    (* IDEA: Compile let to lambdas ? *)

and expression_as_call_head (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident (_, ident, _) ->
    var (longident ident.txt) expr.exp_type
  | _ -> expression expr

and var id ty =
  let ty2 = to_kotlin_type ty in
  let e' = Ident (id, ty2) in
  try
    let (params, ty1) = Compilenv.find_global id in
    if params = [] then [], e'
    else
      let unienv = unify ty1 ty2 in
      let toinst = List.of_seq (TVarSet.to_seq (collect_type_vars ty1)) in
      [], TypeArguments (e', List.map (fun x -> TVarEnv.find x unienv) toinst)
  with Not_found -> [], e'

and function_args = function
  | [] -> [], []
  | (_, None)::tl -> function_args tl
  | (_, Some e)::tl ->
    let stmts1, e' = expression e and stmts2, es' = function_args tl
    in stmts1@stmts2, e'::es'

let is_function (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_function { cases = [_] } -> true
  | _ -> false

let rec expression_as_function (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_function { cases = [{c_lhs = pat; c_guard = None; c_rhs = e} ] } ->
    let (x, destr1) = pattern pat in
    let tx = to_kotlin_type pat.pat_type in
    let (stmts, body) = expression e in
    (x, tx), destr1@stmts@[Return body], (to_kotlin_type (e.exp_type))
  | _ -> invalid_arg "expression_as_function"

let expression_as_main (expr : Typedtree.expression) =
  let fid = Atom.fresh "main" in
  Compilenv.register_main fid;
  let (stmts, e) = expression expr in
  FunctionDecl ([], fid, [], TypeUnit, stmts@([Expression e]))

(** Convert a value binding to a declaration *)
let value_binding (bind : value_binding) =
  match bind.vb_pat.pat_desc with
  | Tpat_any -> expression_as_main bind.vb_expr
  | Tpat_var (_, f) when is_function bind.vb_expr ->
    let (x, tx), stmts, retty = expression_as_function bind.vb_expr in
    let typarams = List.of_seq (TVarSet.to_seq (collect_types_vars (tx::retty::[]))) in
    Compilenv.register_global f.txt (typarams, (FunctionType ([tx], retty)));
    FunctionDecl (typarams, f.txt, [(x, tx)], retty, stmts)
  | Tpat_var (_, x) ->
    let stmts, e = expression bind.vb_expr in
    let ty = to_kotlin_type bind.vb_expr.exp_type in
    (* IDEA: In case we get some statements, we pack everything into a lambda *)
    Compilenv.register_global x.txt ([], ty);
    if stmts = [] then PropertyDecl (x.txt, ty, e)
    else failwith "TODO"

(** Convert a structure item to one or several declarations *)
let structure_item (st : Typedtree.structure_item) =
  match st.str_desc with
  | Tstr_value (_, vals) -> List.map value_binding vals
  | Tstr_eval (e, _) -> [expression_as_main e]
  | Tstr_primitive ({ val_prim = [prim] } as desc) ->
    Compilenv.register_primitive desc.val_name.txt prim; []
  | Tstr_attribute _ -> []

let make_main () =
  let mains = Compilenv.get_mains () in
  FunctionDecl ([], "main", [], TypeUnit,
                List.map (fun f -> Expression
                             (FunCall (Ident (f, (FunctionType ([TypeUnit], TypeUnit))), []))
               ) mains)

let file (st : structure) =
  let decls = List.concat_map structure_item st.str_items in
  { package_header = "mypackage";
    imports = [];
    declarations = decls@[make_main ()]; }
