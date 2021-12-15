open Ast

(* imprime une expression sous forme entierement parenthésée, de façon à
 * ce qu'on puisse facilement verifier si les précédences et associativités
 * demandées sont bien respectées.
 *)
let rec printExpr e =
	match e with
		Id s -> print_string s
		| Cste i -> print_int i
		| Plus(g, d) ->
			print_string "["; printExpr g; print_string " + ";
			printExpr d; print_string "]"
		| Minus (g, d) ->
			print_string "["; printExpr g; print_string " - ";
			printExpr d; print_string "]"
		| Times (g, d) ->
			print_string "["; printExpr g; print_string " * ";
			printExpr d; print_string "]"
		| Div (g, d) ->
			print_string "["; printExpr g; print_string " / ";
			printExpr d; print_string "]"
		| UMinus e -> print_string "[ - ";  printExpr e; print_string "]"
		| Comp(op, g, d) ->
			print_string "["; printExpr g;
			print_string (Utils.string_of_relop op); printExpr d; print_string "]"
		| Ite (si, alors, sinon) ->
			print_string " IF "; printExpr si;
			print_string " THEN "; printExpr alors;
			print_string " ELSE "; printExpr sinon;
			print_endline "]"

let printDecl d =
	print_string d.lhs; print_string " := "; printExpr d.rhs;
	print_newline ()

let printAll ld e =
	List.iter printDecl ld;
	printExpr e;
	print_newline ()

