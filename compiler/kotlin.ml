(** ** Kotlin language *)

(** For now we encode identifier as strings *)
type ident = Ident.t

type type_var = int

type kotlin_type =
  | TypeBool
  | TypeInt
  | TypeDouble
  | TypeString
  | TypeUnit
  | TypeVar of int
  | FunctionType of kotlin_type list * kotlin_type
  | UserType of user_type
  (* TODO  *)

and simple_user_type = (ident * type_arguments)
and user_type = simple_user_type list

and type_arguments = kotlin_type list

type literalConstant =
  | BooleanLit of bool
  | IntegerLit of int
  | HexLit of int
  | BinLit of int
  | RealLit of float
  | StringLit of string
  (* | Null *)
  | Unit

type binop =
  | Conjunction
  | Equality

type parameter = ident * kotlin_type

type classModifier =
  | Data
  | Sealed

(** Expression language *)
type expr =
  | Literal of literalConstant
  | Ident of (Path.t * kotlin_type)
  | FunCall of expr * expr list
  | TypeArguments of expr * type_arguments
  | AnonymousFun of parameter list * statement list
  | Lambda of parameter list * statement list
  | When of expr option * whenEntry list
  | BinOp of (binop * expr * expr)
  | TypeTest of expr * kotlin_type
  | Throw of expr
  | MemberAccess of (expr * ident)
  | ConstructorInvocation of (user_type * expr list)
  (* TODO *)

and whenCondition =
  | Expression of expr
  | WhenTypeTest of kotlin_type

and whenEntry =
  | Condition of whenCondition list * statement list
  | Else of statement list

and statement =
  | Expression of expr
  | Assignment of ident * expr
  (* | LoopStatement of loopstatement *)
  | Return of expr
  | Declaration of declaration

and declaration =
  (* TODO *)
  | FunctionDecl of function_decl
  | PropertyDecl of ident * kotlin_type * expr
  | ClassDecl of class_decl
  | ObjectDecl of object_decl

and function_decl =
  { fund_tparams : type_var list;
    fund_name : ident;
    fund_params : parameter list;
    fund_rettype : kotlin_type;
    fund_body : statement list
  }

and class_decl =
  { cld_modifs : classModifier list;
    cld_name : ident;
    cld_tparams : type_var list;
    cld_constr : (parameter list) option;
    cld_deleg : user_type option;
    cld_body : declaration list;
  }

and object_decl =
  { objd_name : ident;
    objd_deleg : user_type option;
  }

type file = {
  package_header : ident;
  imports : (ident list) list;
  declarations : declaration list;
}

(** Get the type of a literal *)
let type_of_lit = function
  | BooleanLit _ -> TypeBool
  | IntegerLit _ | HexLit _ | BinLit _ -> TypeInt
  | RealLit _ -> TypeDouble
  | StringLit _ -> TypeString
  | Unit -> TypeUnit

module TVarEnv = Map.Make(Int)
let tvar_union = TVarEnv.union (fun _ ty1 _ -> Some ty1)

(** Unify possibly polymorphic type [ty1] with instantiated type [ty2] *)
let rec unify (ty1 : kotlin_type) (ty2 : kotlin_type) =
  match ty1, ty2 with
  | TypeVar x, _ -> TVarEnv.singleton x ty2
  | FunctionType (tins1, tout1), FunctionType (tins2, tout2) ->
    tvar_union (unifys tins1 tins2) (unify tout1 tout2)
  | _, _ -> TVarEnv.empty
and unifys tys1 tys2 =
  List.fold_left2 (fun e ty1 ty2 -> tvar_union e (unify ty1 ty2)) TVarEnv.empty tys1 tys2

module TVarSet = Set.Make(Int)

(** Collect the type variables appearing in a kotlin_type *)
let rec collect_type_vars = function
  | TypeVar x -> TVarSet.singleton x
  | FunctionType (tins, tout) ->
    TVarSet.union (collect_types_vars tins) (collect_type_vars tout)
  | UserType ts ->
    List.fold_left (fun ps (_, tys) -> TVarSet.union ps (collect_types_vars tys)) TVarSet.empty ts
  | _ -> TVarSet.empty
and collect_types_vars tys =
  List.fold_left (fun ps ty -> TVarSet.union ps (collect_type_vars ty)) TVarSet.empty tys
