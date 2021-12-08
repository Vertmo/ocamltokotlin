(** ** Pretty printer for Kotlin *)

open Format
open Kotlin

let print_nothing fmt =
  fprintf fmt ""

let print_separated_list sep p =
  pp_print_list ~pp_sep:(fun p () -> fprintf p sep) p

let print_spaced_list p = print_separated_list "@ " p
let print_double_spaced_list p = print_separated_list " @ " p
let print_comma_list p = print_separated_list ",@ " p
let print_dot_list p = print_separated_list "." p

let print_ident fmt id =
  fprintf fmt "%s" (Ident.name id)

let rec print_path fmt p =
  (* TODO this is probably not correct with modules *)
  Format.fprintf fmt "%s" (Path.last p)
  (* Path.(function *)
  (* | Pident id -> print_ident fmt id *)
  (* | Pdot(p, s) -> Format.fprintf fmt "%a.%s" print_path p s *)
  (* | Papply(p1, p2) -> Format.fprintf fmt "%a(%a)" print_path p1 print_path p2) *)

let print_package_header fmt s =
  fprintf fmt "@[<h 0>package %a;@]"
    print_ident s

let print_import fmt s =
  fprintf fmt "@[<h 0>import %a;@]"
    (print_dot_list print_ident) s

let print_type_var fmt (t : type_var) =
  fprintf fmt "T%d" t

let rec print_type fmt = function
  | TypeBool -> fprintf fmt "Boolean"
  | TypeInt -> fprintf fmt "Int"
  | TypeDouble -> fprintf fmt "Double"
  | TypeString -> fprintf fmt "String"
  | TypeUnit -> fprintf fmt "Unit"
  | TypeVar x -> print_type_var fmt x
  | FunctionType (tins, tout) ->
    fprintf fmt "(%a) -> %a"
      (print_comma_list print_type) tins
      print_type tout
  | UserType tys ->
    fprintf fmt "@[<h 0>%a@]"
      print_user_type tys

and print_simple_user_type fmt (name, tparams) =
    fprintf fmt "%a%a"
      print_ident name
      (if tparams = [] then (fun _ _ -> ())
      else (fun fmt -> fprintf fmt "<%a>" (print_comma_list print_type))) tparams
and print_user_type fmt = print_dot_list print_simple_user_type fmt

let print_parameter fmt (x, ty) =
  fprintf fmt "%a : %a"
    print_ident x
    print_type ty

let print_constructor_parameter fmt (x, ty) =
  fprintf fmt "val %a : %a"
    print_ident x
    print_type ty

let print_deleg fmt =
  Option.fold ~none:(print_nothing fmt) ~some:(fprintf fmt "@ :@ %a()" print_user_type)

let print_constructor fmt =
  Option.fold ~none:(print_nothing fmt)
    ~some:(fprintf fmt "(%a)" (print_comma_list print_constructor_parameter))

let print_class_modifier fmt = function
  | Data -> fprintf fmt "data"
  | Sealed -> fprintf fmt "sealed"

let print_class_modifiers fmt modifs =
  if modifs = [] then fprintf fmt ""
  else fprintf fmt "%a@ "
      (print_spaced_list print_class_modifier) modifs

let print_type_params fmt tparams =
  if tparams = [] then ()
  else fprintf fmt "<%a> " (print_comma_list print_type_var) tparams

let print_literal fmt = function
  | BooleanLit b -> fprintf fmt "%B" b
  | IntegerLit i -> fprintf fmt "%d" i
  | HexLit h -> fprintf fmt "%x" h
  | BinLit b -> failwith "TODO: print_literal : BinLit"
  | RealLit f -> fprintf fmt "%f" f
  | StringLit s -> fprintf fmt "\"%s\"" s
  | Unit -> fprintf fmt "Unit"

let print_binop fmt = function
  | Conjunction -> fprintf fmt "&&"
  | Equality -> fprintf fmt "=="

let rec print_expression fmt = function
  | Ident (x, _) -> print_path fmt x
  | Literal l -> print_literal fmt l
  | FunCall (e, params) ->
    fprintf fmt "@[<hov 2>%a(%a)@]"
      print_expression e
      (print_comma_list print_expression) params
  | TypeArguments (e, params) ->
    fprintf fmt "@[<hov 2>%a<%a>@]"
      print_expression e
      (print_comma_list print_type) params
  | AnonymousFun (xs, body) ->
    fprintf fmt "@[<hov 2>fun(@[<h 0>%a@]) {@ %a@;<0 -2>}@]"
      (print_comma_list print_parameter) xs
      print_statements body
  | Lambda (xs, body) ->
    fprintf fmt "@[<hov 2>{ %a -> %a@;<0 -2>}@]"
      (print_comma_list print_parameter) xs
      print_statements body
  | When (e, entrys) ->
    fprintf fmt "@[<v 2>@[<h 0>when@ %a@] {@ %a@;<0 -2>}@]"
      print_optexpr e
      (print_spaced_list print_whenEntry) entrys
  | BinOp (op, e1, e2) ->
    fprintf fmt "(%a %a %a)"
      print_expression e1
      print_binop op
      print_expression e2
  | TypeTest (e, ty) ->
    fprintf fmt "(%a is %a)"
      print_expression e
      print_type ty
  | Throw e ->
    fprintf fmt "throw %a"
      print_expression e
  | MemberAccess (e, x) ->
    fprintf fmt "%a.%a"
      print_expression e
      print_ident x
  | ConstructorInvocation (x, params) ->
    if List.length params = 0
    then print_user_type fmt x
    else fprintf fmt "%a(%a)"
        print_user_type x
        (print_comma_list print_expression) params

and print_optexpr fmt =
  Option.fold ~none:(print_nothing fmt) ~some:(print_expression fmt)

and print_whenCondition fmt : whenCondition -> _ = function
  | Expression e -> print_expression fmt e
  | WhenTypeTest ty -> fprintf fmt "is %a" print_type ty

and print_whenEntry fmt = function
  | Condition (c, block) ->
    fprintf fmt "%a -> @[<v 2>{@ %a@;<0 -2>}@]"
      (print_comma_list print_whenCondition) c
      print_statements block
  | Else block ->
    fprintf fmt "else -> @[<v 2>{@ %a@;<0 -2>}@]"
      print_statements block

and print_statement fmt = function
  | Expression e -> fprintf fmt "%a" print_expression e
  | Assignment (x, e) -> fprintf fmt "%a = %a"
                           print_ident x
                           print_expression e
  | Return e -> fprintf fmt "return %a" print_expression e
  | Declaration d -> print_declaration fmt d

and print_statements fmt stmts =
  print_separated_list ";@ " print_statement fmt stmts

and print_declaration fmt = function
  | PropertyDecl (x,  ty, e) ->
    fprintf fmt "@[<h 0>var %a : %a = %a@]"
      print_ident x
      print_type ty
      print_expression e
  | FunctionDecl fd ->
    fprintf fmt "@[<v 2>@[<h 0>fun@ %a%a(%a) : %a@] {@ %a@;<0 -2>}@]"
      print_type_params fd.fund_tparams
      print_ident fd.fund_name
      (print_comma_list print_parameter) fd.fund_params
      print_type fd.fund_rettype
      print_statements fd.fund_body
  | ClassDecl cd ->
    fprintf fmt "@[<v 2>@[<h 0>%aclass@ %a%a%a%a@] {@ %a@;<0 -2>}@]"
      print_class_modifiers cd.cld_modifs
      print_ident cd.cld_name
      print_type_params cd.cld_tparams
      print_constructor cd.cld_constr
      print_deleg cd.cld_deleg
      (print_double_spaced_list print_declaration) cd.cld_body
  | ObjectDecl od ->
    fprintf fmt "@[<h 0>object@ %a%a@]"
      print_ident od.objd_name
      print_deleg od.objd_deleg

let print_file fmt file =
  fprintf fmt "@[<v 0>%a@ %a@ %a@;@]"
    print_package_header file.package_header
    (print_spaced_list print_import) file.imports
    (print_double_spaced_list print_declaration) file.declarations
