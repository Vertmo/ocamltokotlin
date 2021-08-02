(** ** Pretty printer for Kotlin *)

open Format
open Kotlin

let print_spaced_list p =
  pp_print_list ~pp_sep:(fun p () -> fprintf p "@ ") p

let print_double_spaced_list p =
  pp_print_list ~pp_sep:(fun p () -> fprintf p "@ @ ") p

let print_comma_list p =
  pp_print_list ~pp_sep:(fun p () -> fprintf p ",@ ") p

let print_package_header fmt s =
  fprintf fmt "@[<h 0>package %s;@]" s

let print_import fmt s =
  fprintf fmt "@[<h 0>import %s;@]" s

let print_type_var fmt (t : type_var) =
  fprintf fmt "T%d" t

let rec print_type fmt = function
  | TypeBool -> fprintf fmt "Bool"
  | TypeInt -> fprintf fmt "Int"
  | TypeString -> fprintf fmt "String"
  | TypeUnit -> fprintf fmt "Unit"
  | TypeVar x -> print_type_var fmt x
  | FunctionType (tins, tout) ->
    fprintf fmt "(%a) -> %a"
      (print_comma_list print_type) tins
      print_type tout

let print_ident fmt = fprintf fmt "%s"

let print_parameter fmt (x, ty) =
  fprintf fmt "%s : %a"
    x
    print_type ty

let print_type_params fmt tparams =
  if tparams = [] then ()
  else fprintf fmt "<%a> " (print_comma_list print_type_var) tparams

let print_literal fmt = function
  | BooleanLit b -> fprintf fmt "%B" b
  | IntegerLit i -> fprintf fmt "%d" i
  | HexLit h -> fprintf fmt "%x" h
  | RealLit f -> fprintf fmt "%f" f
  | StringLit s -> fprintf fmt "\"%s\"" s
  (* | Null -> fprintf fmt "null" *)
  | Unit -> fprintf fmt "Unit"

let rec print_expression fmt = function
  | Ident (x, _) -> fprintf fmt "%s" x
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

and print_statement fmt = function
  | Expression e -> fprintf fmt "%a" print_expression e
  | Return e -> fprintf fmt "return %a" print_expression e

and print_statements fmt stmts =
  pp_print_list ~pp_sep:(fun p () -> fprintf p ";@ ") print_statement fmt stmts

let print_declaration fmt = function
  | PropertyDecl (x,  ty, e) ->
    fprintf fmt "@[<h 0>var %s : %a = %a;@]"
      x
      print_type ty
      print_expression e
  | FunctionDecl (tparams, f, params, retty, body) ->
    fprintf fmt "@[<v 2>@[<h 0>fun@ %a%s(%a) : %a@] {@ %a@;<0 -2>}@]"
      print_type_params tparams
      f
      (print_comma_list print_parameter) params
      print_type retty
      (print_spaced_list print_statement) body

let print_file fmt file =
  fprintf fmt "@[<v 0>%a@ %a@ %a@;@]"
    print_package_header file.package_header
    (print_spaced_list print_import) file.imports
    (print_double_spaced_list print_declaration) file.declarations
