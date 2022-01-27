type opComp =
	Eq | Neq | Lt | Le | Gt | Ge
;;

type expr =
	Id of string
	| Result
	| This
	| Super
	| Cste of int
	| String of string
	| Cast of string * expr
	| Instantiation of string * expr list
	| FieldAccess of expr * string
	| MethodCall of expr * string * expr list
	| StaticFieldAccess of string * string
	| StaticMethodCall of string * string * expr list
	| Comp of opComp * expr * expr
	| Plus of expr * expr
	| Minus of expr * expr
	| Times of expr * expr
	| Div of expr * expr
	| Concat of expr * expr
	| UMinus of expr
;;

type constrParam = {
	var: bool;
	param: string list;
	classname_constr_param: string;
}
;;

type methodParam = {
	param: string list;
	classname_method_param: string;
}
;;

type instr =
	Expr of expr
	| Return
	| Assignment of expr * expr
	| Ite of expr * instr * instr
	| BlockInstr of blockType
and
blockType =
	| Block of instr list
	| BlockVar of methodParam list * instr list
;;

type classElem =
	Field of bool * string list * string
	| Constr of string * constrParam list * ((string * expr list) option) * blockType
	| SimpleMethod of bool * bool * string * methodParam list * string * expr
	| ComplexMethod of bool * bool * string * methodParam list * string option * blockType
;;

type decl = {
	classname: string;
	lparam: constrParam list;
	superClassOpt: string option;
	ce: classElem list;
}
;;

type prog = {
	classes: decl list;
	block: blockType;
}
;;

exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
