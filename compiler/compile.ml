(** Compiler functions *)

open Utils
open Typedtree
open Kotlin

let exnid = Ident.create_predef "Exception"
let mainid = Ident.create_predef "main"
let packageid = Ident.create_predef "mypackage"

let todo func case = failwith (Printf.sprintf "TODO: %s : %s" func case)

let rec type_expr (ty : Types.type_expr) =
  match ty.desc with
  | Tvar _ -> TypeVar ty.id
  | Tarrow (_, tin, tout, _) ->
    FunctionType ([type_expr tin], type_expr tout)
  | Tconstr (path, [], _) when Path.name path = "unit" -> TypeUnit
  | Tconstr (path, [], _) when Path.name path = "string" -> TypeString
  | Tconstr (path, [], _) when Path.name path = "int" -> TypeInt
  | Tconstr (path, [], _) when Path.name path = "bool" -> TypeBool
  | Tconstr (path, params, _) -> UserType [(Path.head path, List.map type_expr params)]
  | Tlink ty -> type_expr ty
  | _ -> Printtyp.raw_type_expr Format.std_formatter ty; failwith "TODO"

let constant = function
  | Asttypes.Const_int i -> IntegerLit i
  | Asttypes.Const_string (s, _, _) -> StringLit s
  | _ -> todo "constant" ""

let extract_user_type = function
  | UserType ty -> ty
  | _ -> invalid_arg "extract_user_type"

let constructor constr params =
  match Types.(constr.cstr_name) with
  | "()" -> Literal Unit
  | "true" -> Literal (BooleanLit true)
  | "false" -> Literal (BooleanLit false)
  | name ->
    let ty = extract_user_type (type_expr constr.cstr_res) in
    let ty = List.map (fun (x, _) -> (x, [])) ty in
    let params = if params = [] then [Literal Unit] else params in
    ConstructorInvocation (ty@[(Ident.create_predef name, [(* TODO? *)])], params)

let rec get_constr (ty : Types.type_expr) : Ident.t =
  match ty.desc with
  | Tlink ty -> get_constr ty
  | Tconstr (c, _, _) -> Path.head c
  | _ -> invalid_arg "get_constr"

(** Bring the "or" case of the pattern matching up in the match.
 ** Simplifies compilation of the pattern matching later on
 **
 ** match x with
 ** A (B v | C v) -> ...
 **
 ** becomes
 **
 ** match x with
 ** A (B v) | A (C v) *)
let rec value_pattern_distr_or pat : (value general_pattern) list =
  match pat.pat_desc with
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_, None, _) -> [pat]
  | Tpat_alias (p, id, x) ->
    List.map (fun p -> { pat with pat_desc = Tpat_alias (p, id, x) }) (value_pattern_distr_or p)
  | Tpat_construct (x, constr, pats) ->
    List.map (fun pats -> { pat with pat_desc = Tpat_construct (x, constr, pats) }) (value_patterns_distr_or pats)
  | Tpat_tuple pats ->
    List.map (fun pats -> { pat with pat_desc = Tpat_tuple pats }) (value_patterns_distr_or pats)
  | Tpat_variant (lab, Some p, row) ->
    List.map (fun p -> { pat with pat_desc = Tpat_variant (lab, Some p, row) }) (value_pattern_distr_or p)
  | Tpat_record _ -> todo "value_pattern_distr_or" "record"
  | Tpat_array pats ->
    List.map (fun pats -> { pat with pat_desc = Tpat_array pats }) (value_patterns_distr_or pats)
  | Tpat_lazy pat ->
    List.map (fun p -> { pat with pat_desc = Tpat_lazy pat }) (value_pattern_distr_or pat)
  | Tpat_or (pat1, pat2, _) ->
    (value_pattern_distr_or pat1)@(value_pattern_distr_or pat2)

(* This is where the combinatory explosion happens :p *)
and value_patterns_distr_or pats : ((value general_pattern) list) list  =
  match pats with
  | [] -> [[]]
  | hd::tl ->
    let hd = value_pattern_distr_or hd and tl = value_patterns_distr_or tl in
    List.concat_map (fun hd -> List.map (fun tl -> hd::tl) tl) hd

and computation_pattern_distr_or pat : (computation general_pattern) list =
  match pat.pat_desc with
  | Tpat_value v ->
    List.map as_computation_pattern (value_pattern_distr_or (v :> value general_pattern))
  | Tpat_exception p ->
    List.map (fun p -> { pat with pat_desc = Tpat_exception p }) (value_pattern_distr_or p)
  | Tpat_or (pat1, pat2, _) ->
    (computation_pattern_distr_or pat1)@(computation_pattern_distr_or pat2)

(** Conditions and bindings induced by a pattern matching.
 ** Doesn't support disjunctive patterns *)
let rec value_pattern (matched : expr) (pat : value general_pattern) : (expr list * statement list) =
  match pat.pat_desc with
  | Tpat_any -> [], []
  | Tpat_var (x, _) -> [], [Declaration (PropertyDecl (x, type_expr pat.pat_type, matched))]
  | Tpat_alias (pat, x, _) ->
    let (conds, binds) = value_pattern matched pat in
    conds, (Declaration (PropertyDecl (x, type_expr pat.pat_type, matched)))::binds
  | Tpat_constant cst ->
    [BinOp (Equality, matched, Literal (constant cst))], []
  | Tpat_construct (_, constr, vs) ->
    let typetest = TypeTest (matched, UserType [(get_constr constr.cstr_res, []);(Ident.create_predef constr.cstr_name, [])]) in
    let pats = List.mapi (fun i pat -> value_pattern (MemberAccess (matched, Ident.create_predef ("field"^(string_of_int i)))) pat) vs in
    typetest::(List.concat_map fst pats), List.concat_map snd pats
  | Tpat_tuple _ -> todo "pattern" "tuple"
  | Tpat_variant _ -> todo "pattern" "variant"
  | Tpat_record _ -> todo "pattern" "record"
  | Tpat_array _ -> todo "pattern" "array"
  | Tpat_lazy _ -> todo "pattern" "lazy"
  | Tpat_or (pat1, pat2, _) -> invalid_arg "value_pattern"
    (* (value_pattern matched pat1)@(value_pattern matched pat2) *)

and computation_pattern e (pat : computation general_pattern) =
  match pat.pat_desc with
  | Tpat_value v -> value_pattern e (v :> value general_pattern)
  | Tpat_exception _ -> todo "computation_pattern" "exception"
  | Tpat_or (pat1, pat2, _) -> invalid_arg "value_pattern"
    (* (computation_pattern e pat1) @ (computation_pattern e pat2) *)

let rec conj_cond (es : expr list) =
  match es with
  | [] -> Literal (BooleanLit true)
  | [e] -> e
  | hd::tl -> BinOp (Conjunction, hd, conj_cond tl)

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
       let tmp = Ident.create_local "tmp" in
       Lambda ([(tmp, tx)], [Expression (FunCall (e, [Ident (Path.Pident tmp, tx)]))])
     with Not_found -> e)
  | _ -> e

(** Generate the expression for throwing an exception with `msg` *)
let throw_exception msg =
  Throw (FunCall (Ident ((Path.Pident exnid), UserType [(exnid, [])]), [Literal (StringLit msg)]))

let rec expression (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_constant c ->
    [], Literal (constant c)
  | Texp_construct (_, constr, params) ->
    let es = List.map expression params in
    List.concat_map fst es, constructor constr (List.map snd es)
  | Texp_apply (e, args) ->
    let (stmts1, e') = expression_as_call_head e in
    let (stmts2, args') = function_args args in
    stmts2, make_apply e' args'
    (* stmts2, FunCall (e', args') *)
  | Texp_ident (pt, _, _) ->
    let stmts, e = var pt expr.exp_type in
    stmts, eta_expand e
  | Texp_sequence (e1, e2) ->
    let stmts1, e1' = expression e1 and stmts2, e2' = expression e2 in
    stmts1@[Expression e1']@stmts2, e2'
  | Texp_function { param; cases = [{ c_lhs = pat; c_guard = None; c_rhs = e }]} ->
    let tx = type_expr pat.pat_type in
    let (stmts1, e') = expression e in
    [], Lambda ([(param, tx)], stmts1@[Expression e'])
  | Texp_function { param; cases } ->
    let tx = type_expr ((List.hd cases).c_lhs.pat_type) in
    let cases = List.concat_map (match_value (Ident (Path.Pident param, tx))) cases in
    [], Lambda ([(param, tx)], [Return (When (None, add_else_case cases))])
  (* TODO complete function *)
  | Texp_match (e, cases, part) ->
    let ty = type_expr e.exp_type in
    let stmts, e' = expression e in
    let x = Ident.create_local "mat" in
    let assign = Declaration (PropertyDecl (x, ty, e')) in
    let cases = List.concat_map (match_computation (Ident (Path.Pident x, ty))) cases in
    stmts@[assign],
    When (None, add_else_case cases)

and match_value e (c : value Typedtree.case) =
  let (stms, e') = expression c.c_rhs in
  let pats = value_pattern_distr_or c.c_lhs in
  (* TODO factorise code *)
  List.map (fun pat ->
      let (cond, binds) = value_pattern e pat in
      Condition ([Expression (conj_cond cond)], binds@stms@[Expression e'])
    ) pats

and match_computation e (c : computation Typedtree.case) =
  let (stms, e') = expression c.c_rhs in
  let pats = computation_pattern_distr_or c.c_lhs in
  (* TODO factorise code *)
  List.map (fun pat ->
      let (cond, binds) = computation_pattern e pat in
      Condition ([Expression (conj_cond cond)], binds@stms@[Expression e'])
    ) pats

and make_last_case_else = function
  | [] -> invalid_arg "make_last_case_else"
  | Condition (_, e)::[] -> Else e::[]
  | hd::tl -> hd::make_last_case_else tl

and add_else_case = function
  | [] -> [Else [Expression (throw_exception "pattern-matching failure")]]
  | hd::tl -> hd::add_else_case tl

and expression_as_call_head (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident (pt, _, _) ->
    var pt expr.exp_type
  | _ -> expression expr

and var (id : Path.t) ty =
  let ty2 = type_expr ty in
  match (Compilenv.get_primitive_opt id) with
  | Some prim -> [], Ident (Path.Pident (Ident.create_predef prim), ty2)
  | None ->
    let e' = Ident (id, ty2) in
    (try
       let ty1 = type_expr (Compilenv.find_global id) in
       let unienv = unify ty1 ty2 in
       let toinst = List.of_seq (TVarSet.to_seq (collect_type_vars ty1)) in
       if toinst = [] then [], e'
       else [], TypeArguments (e', List.map (fun x -> TVarEnv.find x unienv) toinst)
     with Not_found -> [], e')

and function_args = function
  | [] -> [], []
  | (_, None)::tl -> function_args tl
  | (_, Some e)::tl ->
    let stmts1, e' = expression e and stmts2, es' = function_args tl
    in stmts1@stmts2, e'::es'

let is_function (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_function _ -> true
  | _ -> false

let rec expression_as_function (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_function { param; cases = [{c_lhs = pat; c_guard = None; c_rhs = e} ] } ->
    let tx = type_expr pat.pat_type in
    let (stmts, body) = expression e in
    (param, tx), stmts@[Return body], (type_expr (e.exp_type))
  | Texp_function { param; cases } ->
    let tx = type_expr ((List.hd cases).c_lhs.pat_type) in
    let retty =type_expr ((List.hd cases).c_rhs.exp_type) in
    let cases = List.concat_map (match_value (Ident (Path.Pident param, tx))) cases in
    (param, tx), [Return (When (None, add_else_case cases))], retty
  | _ -> invalid_arg "expression_as_function"

let expression_as_main (expr : Typedtree.expression) =
  let fid = Ident.create_local "main" in
  Compilenv.register_main fid;
  let (stmts, e) = expression expr in
  FunctionDecl {
    fund_tparams = [];
    fund_name = fid;
    fund_params = [];
    fund_rettype = TypeUnit;
    fund_body = stmts@[Expression e]
  }

(** Convert a value binding to a declaration *)
let value_binding (bind : value_binding) =
  match bind.vb_pat.pat_desc with
  | Tpat_any -> expression_as_main bind.vb_expr
  | Tpat_var (f, _) when is_function bind.vb_expr ->
    let (x, tx), stmts, retty = expression_as_function bind.vb_expr in
    let typarams = List.of_seq (TVarSet.to_seq (collect_types_vars (tx::retty::[]))) in
    (* Compilenv.register_global f (typarams, (FunctionType ([tx], retty))); *)
    FunctionDecl {
      fund_tparams = typarams;
      fund_name = f;
      fund_params = [(x, tx)];
      fund_rettype = retty;
      fund_body = stmts
    }
  | Tpat_var (x, _) ->
    let stmts, e = expression bind.vb_expr in
    let ty = type_expr bind.vb_expr.exp_type in
    (* IDEA: In case we get some statements, we pack everything into a lambda *)
    (* Compilenv.register_global x ([], ty); *)
    if stmts = [] then PropertyDecl (x, ty, e)
    else todo "value_binding" "Tpat_var"
  | _ -> todo "value_binding" "other"

let constructor_declaration clty (decl : Types.constructor_declaration) : declaration =
  let cld_name = Ident.create_predef (Ident.name decl.cd_id) in
  match decl.cd_args with
  | Cstr_tuple [] | Cstr_record [] ->
    let tvars = List.of_seq (TVarSet.to_seq (collect_type_vars (UserType clty))) in
    ClassDecl {
      cld_modifs = [Data];
      cld_name;
      cld_tparams = tvars;
      cld_constr = Some [(Ident.create_persistent "dummy", TypeUnit)];
      cld_deleg = Some clty;
      cld_body = []
    }
  | Cstr_tuple args ->
    let tvars = List.of_seq (TVarSet.to_seq (collect_types_vars (UserType clty::List.map type_expr args))) in
    ClassDecl {
      cld_modifs = [Data];
      cld_name;
      cld_tparams = tvars;
      cld_constr = Some (List.mapi (fun i ty -> Ident.create_predef ("field"^(string_of_int i)), type_expr ty) args);
      cld_deleg = Some clty;
      cld_body = []
    }
  | _ -> todo "constructor_declaration" "Cstr_record"

let type_declaration (decl : Typedtree.type_declaration) : declaration =
  match decl.typ_type.type_kind with
  | Types.Type_variant constrs ->
    let clid = decl.typ_id in
    let tparams = List.map (fun (ty, _) -> type_expr ty.ctyp_type) decl.typ_params in
    let tvars = List.of_seq (TVarSet.to_seq (collect_types_vars tparams)) in
    let clty = [(clid, List.map (fun x -> TypeVar x) tvars)] in
    ClassDecl {
      cld_modifs = [Sealed];
      cld_name = clid;
      cld_tparams = tvars;
      cld_constr = None;
      cld_deleg = None;
      cld_body = List.map (constructor_declaration clty) constrs
    }
  | _ -> todo "type_declaration" "other"

(** Convert a structure item to one or several declarations *)
let structure_item (st : Typedtree.structure_item) =
  match st.str_desc with
  | Tstr_value (_, vals) -> List.map value_binding vals
  | Tstr_eval (e, _) -> [expression_as_main e]
  | Tstr_primitive _ -> []
  | Tstr_type (_, decls) ->
    List.map type_declaration decls
  | Tstr_attribute _ -> []
  | Tstr_open _ -> [] (* TODO? *)
  | _ -> todo "structure_item" "other"

let make_main () =
  let mains = Compilenv.get_mains () in
  FunctionDecl {
    fund_tparams = [];
    fund_name = mainid;
    fund_params = [];
    fund_rettype = TypeUnit;
    fund_body =
      List.map (fun f ->
          Expression
            (FunCall (Ident (Path.Pident f, (FunctionType ([TypeUnit], TypeUnit))), []))
        ) mains
  }

let file (st : structure) =
  let decls = List.concat_map structure_item st.str_items in
  { package_header = packageid;
    imports = [];
    declarations = decls@[make_main ()]; }
