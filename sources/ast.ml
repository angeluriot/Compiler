type opComp =
	Eq | Neq | Lt | Le | Gt | Ge;;

type expr =
	Id of string
	| Cste of int
	| String of string
	| Cast of string * expr
	| Instantiation of string * expr list
	| FieldAccess of expr * string
	| MethodCall of expr * string * expr list
	| Comp of opComp * expr * expr
	| Plus of expr * expr
	| Minus of expr * expr
	| Times of expr * expr
	| Div of expr * expr
	| Concat of expr * expr
	| UMinus of expr;;

type 'a option = Sum of 'a | None;;

type constrParam = {
	var: bool;
	param: string list;
	classname: string;
};;

type methodParam = {
	param: string list;
	classname: string;
};;

type instr =
	Expr of expr
	| Return
	| Assignment of expr * expr
	| Ite of expr * instr * instr;;

type blockType = {
	instrs: instr list;
};;

type classElem =
	Field of bool * string * string
	| Constr of string * constrParam list * string option * instr list
	| SimpleMethod of bool * bool * string * constrParam list * string * expr
	| ComplexMethod of bool * bool * string * constrParam list * string * blockType;;

type decl = {
	classname: string;
	lparam: constrParam list;
	superClassOpt: string option;
	ce: classElem list;
};;

type cast = {
	className: string;
	expression: expr;
};;

type prog = {
	classes: decl list;
	instrs: instr list
};;
