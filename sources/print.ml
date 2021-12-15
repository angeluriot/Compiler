open Ast

let printOp o =
	match o with
	Eq		-> Printf.printf " = "
	| Neq	-> Printf.printf " <> "
	| Lt	-> Printf.printf " < "
	| Le	-> Printf.printf " <= "
	| Gt	-> Printf.printf " > "
	| Ge	-> Printf.printf " >= "

let rec printExpr e =
	match e with
	Id(i)			-> Printf.printf "%s" i
	| Cste(c)		-> Printf.printf "%i" c
	| UMinus(e)		-> begin Printf.printf " - "; printExpr e end
	| Plus(g, d)	-> begin printExpr g; Printf.printf " + "; printExpr d end
	| Minus(g, d)	-> begin printExpr g; Printf.printf " - "; printExpr d end
	| Times(g, d)	-> begin printExpr g; Printf.printf " * "; printExpr d end
	| Div(g, d)		-> begin printExpr g; Printf.printf " / "; printExpr d end
	| Comp(g, o, d)	-> begin printExpr g; printOp o; printExpr d end
	| Ite(c, t, e)	->
		begin
			Printf.printf "if ";
			printExpr c;
			Printf.printf "\n\tthen ";
			printExpr t;
			Printf.printf "\nelse\n\t";
			printExpr e
		end

let printDecl d =
	begin
		Printf.printf "%s" d.lhs;
		Printf.printf " := ";
		printExpr d.rhs
	end

let print decls exp =
	List.iter printDecl decls;
	printExpr exp;
;;
