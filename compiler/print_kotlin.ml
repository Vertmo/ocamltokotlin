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

let print_type fmt = function
  | Tint -> fprintf fmt "int"

let print_parameter fmt (ty, x) =
  fprintf fmt "%s : %a"
    x
    print_type ty

let print_literal fmt = function
  | BooleanLit b -> if b then fprintf fmt "true" else fprintf fmt "false"
  | IntegerLit i -> fprintf fmt "%d" i
  | StringLit s -> fprintf fmt "\"%s\"" s

let rec print_expression fmt = function
  | Ident x -> fprintf fmt "%s" x
  | Literal l -> print_literal fmt l
  | FunCall (e, params) ->
    fprintf fmt "%a(%a)"
      print_expression e
      (print_comma_list print_expression) params

let print_statement fmt = function
  | Expression e -> fprintf fmt "%a;" print_expression e

let print_declaration fmt = function
  | PropertyDecl (x, e) ->
    fprintf fmt "@[<h 0>var@ %s@ =@ %a;@]"
      x
      print_expression e
  | FunctionDecl (f, params, body) ->
    fprintf fmt "@[<v 2>fun %s(%a) {@ %a@;<0 -2>}@]" f
      (print_comma_list print_parameter) params
      (print_spaced_list print_statement) body

let print_file fmt file =
  fprintf fmt "@[<v 0>%a@ %a@ %a@;@]"
    print_package_header file.package_header
    (print_spaced_list print_import) file.imports
    (print_double_spaced_list print_declaration) file.declarations
