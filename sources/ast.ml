type opComp =
	Eq | Neq | Lt | Le | Gt | Ge

type expType =
	Id of string
	| Cste of int
	| Plus of expType * expType
	| Minus of expType * expType
	| Times of expType * expType
	| Div of expType * expType
	| UMinus of expType
	| Comp of opComp * expType * expType
	| Ite of expType * expType * expType


type decl = {
	lhs: string;
	rhs: expType;
}

type progType = decl list * expType

exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
