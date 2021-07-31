(** ** Kotlin language *)

(** For now we encode identifier as strings *)
type ident = string

type kotlin_type =
  | Tint
  (* TODO  *)

type literalConstant =
  | BooleanLit of bool
  | IntegerLit of int
  | HexLit of int
  | BinLit of int
  | RealLit of float
  | Null
  | StringLit of string

(** Expression language *)
type expression =
  | Literal of literalConstant
  | Ident of ident
  | FunCall of expression * expression list
  (* TODO *)

(** In the subset of the language we use, we only assign to identifiers (for now ?) *)
type assignment = (ident * expression)

type parameter = kotlin_type * ident

type statement =
  | Assignment of assignment
  | LoopStatement of loopstatement
  | Expression of expression

and loopstatement = unit (* TODO *)

and declaration =
  (* TODO *)
  | FunctionDecl of ident * parameter list * statement list
  | PropertyDecl of ident * expression

type file = {
  package_header : ident;
  imports : ident list;
  declarations : declaration list;
}
