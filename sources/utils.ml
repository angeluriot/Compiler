(* on aurait pu mettre le contenu directement dans le fichier print.ml
 * Ca sert juste à montrer où mettre dans le Makefile un ficherui qui va
 * définir des fonctions basiques et qui ne dépend pas des autres fichiers
 *)

open Ast

let string_of_relop (op: Ast.opComp) =
	match op with
		Eq -> "="
	| Neq -> "<>"
	| Lt -> "<"
	| Le -> "<="
	| Gt -> ">"
	| Ge -> ">="
