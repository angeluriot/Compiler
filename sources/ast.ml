type opComp = Eq | Neq | Lt | Le | Gt | Ge

type expType =
	Id of string
	| Cste of int
	| UMinus of expType
	| Plus of expType * expType
	| Minus of expType * expType
	| Times of expType * expType
	| Div of expType * expType
	| Comp of expType * opComp * expType
	| Ite of expType * expType * expType


type decl = {
	lhs: string;
	rhs: expType;
}

(* si on veut pouvoir distinguer plusieurs sortes d'erreur qui empechent la
 * poursuite de la compilation
 *)

exception VC_error of string (* erreur contextuelle *)
exception Run_error of string (* erreur a l'execution pour un interprete *)
exception Internal_error of string
