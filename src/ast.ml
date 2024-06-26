(** The abstract syntax tree type. *)

(******************************************************************************
   These types (id, handle, uop, bop) are used by the parser and type-checker.
   You do not want to change them.
 ******************************************************************************)

type id = string
type handle = int

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Cat
  | Pipe
  | Cons
  | Assign
  | Bind

and uop =
  | Neg
  | Not
  | Ref
  | Deref

(******************************************************************************
   [pat] is the type of the AST for patterns. You may implement
   this type however you wish. Look at the formal semantics and think about other
   AST types we have worked with for inspiration.
 ******************************************************************************)

type pat = 
  |PInt of int
  |PUnit of unit
  |PString of string
  |PBool of bool
  |PWildcard
  |PVar of id
  |PPair of pat * pat
  |PCons of pat * pat
  |PNil of unit


(******************************************************************************
   [expr] is the type of the AST for expressions. You may implement
   this type however you wish.  Use the example interpreters seen in
   the textbook as inspiration.
 ******************************************************************************)

type expr = 
  |Int of int
  |Unit of unit
  |String of string
  |Bool of bool
  |Binop of bop * expr * expr
  |Let of pat * expr * expr
  |Var of id
  |Fun of pat * expr 
  |App of expr * expr
  |If of expr * expr * expr
  |Pair of expr * expr 
  |Nil of unit
  |Seq of expr * expr
  |Uop of uop * expr
  |Self of unit

(******************************************************************************
   [defn] is the type of the AST for definitions. You may implement this type
   however you wish.  There are only two kinds of definition---the let
   definition and the let [rec] definition---so this type can be quite simple.
 ******************************************************************************)
and defn = 
  |DLet of pat * expr

(******************************************************************************
   [prog] is the type of the AST for an RML program. You should 
   not need to change it.
 ******************************************************************************)

type prog = defn list
