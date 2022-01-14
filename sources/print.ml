open Ast

let printExpr e = ()
;;

let opCompToString op =	match op with
	| Eq	-> "eq"
	| Neq	-> "neq"
	| Lt 	-> "lt"
	| Le 	-> "le"
	| Gt 	-> "gt"
	| Ge 	-> "ge"
;;

(* imprime une expression sous forme entierement parenthésée, de façon à
 * ce qu'on puisse facilement verifier si les précédences et associativités
 * demandées sont bien respectées.
 *)
let rec printExpr e =
	let printBinOp lhs rhs op =
		Printf.printf "[";
		printExpr lhs;
		Printf.printf " [token: %s] " op;
		printExpr rhs;
		Printf.printf "]"
	in
	let rec printLExprParam l = match l with
		| [] -> ()
		| [e] -> printExpr e
		| e :: s -> printExpr e;
			Printf.printf " [token: comma] ";
			printLExprParam s
	in
	match e with
	| Id			s				-> Printf.printf "[token: id: %s]" s
	| Cste			e				-> Printf.printf "[token: cste: %i]" e
	| String 		s				-> Printf.printf "[[token: double-quote] [token: string: %s] [token: double-quote]]" s
	| Cast			(classname, e) 	-> Printf.printf "[[token: lparen] [token: classname: %s] " classname;
										printExpr e;
										Printf.printf "]"
	| Instantiation	(classname, el) -> Printf.printf "[[token: new] [token: classname: %s] [token: lparen] " classname;
										printLExprParam el;
										Printf.printf "[token: rparen]]"
	| FieldAccess	(e, s)			-> Printf.printf "[";
										printExpr e;
										Printf.printf " [token: dot] [token: id: %s]]" s
	| MethodCall	(e, s, el)		-> Printf.printf "[";
										printExpr e;
										Printf.printf " [token: dot] [token: id: %s] [token: lparen] " s;
										printLExprParam el;
										Printf.printf " [token: rparen]]"
	| Comp			(op, lhs, rhs)	-> printBinOp lhs rhs (opCompToString op)
	| Plus			(lhs, rhs)		-> printBinOp lhs rhs "plus"
	| Minus			(lhs, rhs)		-> printBinOp lhs rhs "minus"
	| Times			(lhs, rhs)		-> printBinOp lhs rhs "times"
	| Div			(lhs, rhs)		-> printBinOp lhs rhs "div"
	| Concat		(lhs, rhs)		-> printBinOp lhs rhs "concat"
	| UMinus		e				-> Printf.printf "[[token: minus] ";
										printExpr e;
										Printf.printf "]"
;;

(*
	Affiche le paramètre d'un constructeur d'une classe
*)
let printConstrParam c =
	Printf.printf "[";
	if c.var = true then Printf.printf "[token: var] [params: ";
	match c.param with
	| []		-> failwith "Should not happen in print.ml"
	| [a]		-> Printf.printf "%s " a
	| a :: b	-> Printf.printf "%s " a;
					List.iter (fun s -> Printf.printf ", %s" s) b;
					Printf.printf "] : [token: classname: %s]]" c.classname
;;

(*
	Affiche le paramètre d'une méthode d'une classe
*)
let printMethodParam m =
	Printf.printf "[";
	Printf.printf "[params: ";
	match m.param with
	| []		-> failwith "Should not happen in print.ml"
	| [a]		-> Printf.printf "%s " a
	| a :: b	-> Printf.printf "%s " a;
					List.iter (fun s -> Printf.printf ", %s" s) b;
					Printf.printf "] : [token: classname: %s]]" m.classname
;;

(*
	Affiche une instruction
*)
let rec printInstr i =
	match i with
	| Expr e ->
		Printf.printf "[";
		printExpr e;
		Printf.printf "] [token: semi-colon]]\n"
	| Return ->
		Printf.printf "[[token: return] [token : semi-colon]]\n"
	| Assignment (e1, e2) ->
		Printf.printf "[";
		printExpr e1;
		Printf.printf " [token: assign] ";
		printExpr e2;
		Printf.printf "] [token: semi-colon]]\n";
	| Ite (i, t, e) ->
		Printf.printf "[[token: if] ";
		printExpr i;
		Printf.printf " [token: then] ";
		printInstr t;
		Printf.printf " [token: else] ";
		printInstr e;
		Printf.printf " [token: semi-colon]]\n"
;;

(*
	Affiche un bloc potentiellement vide d'instructions
*)
let printBlocType b = 
    Printf.printf "[[token: lbrace]\n";
    match b with
    | Block li -> List.iter (fun i -> printInstr i) li
	| BlockVar (methodParams, li) ->
        List.iter (fun p -> printMethodParam p) methodParams;
        Printf.printf " [token: is] ";
        List.iter (fun i -> printInstr i) li;
	Printf.printf "[token: rbrace]]\n"
;;

let rec printConstrLParam l = match l with
	| [] -> ()
	| [p] -> printConstrParam p
	| p :: s ->
		printConstrParam p;
		Printf.printf "[token: comma] ";
		printConstrLParam s
;;

let printMethodWithoutInstr isStatic isOverridden name lparam classname =
	Printf.printf "[token: def] ";
	if isStatic then Printf.printf "[token: static] ";
	if isOverridden then Printf.printf "[token: override] ";
	Printf.printf "[token: id: %s] " name;
	Printf.printf "[token: lparen] ";
	printConstrLParam lparam;
	Printf.printf "[token: rparen] [token: colon] [token: classname: %s] [token: assign] " classname
;;

let printClassElem e = match e with
	| Field (isStatic, name, classname) ->
		(
			Printf.printf "[[token: var] ";
			if isStatic then Printf.printf "[token: static] ";
			Printf.printf "[token: id: %s] " name;
			Printf.printf "[token: colon] [token: classname: %s] [token: semi-colon]]\n" classname
		)
	| Constr (classname, lparam, superClassOpt, bloc) ->
		(
			Printf.printf "[[token: def] [token: classname: %s] [token: lparen] " classname;
			printConstrLParam lparam;
			Printf.printf " [token: rparen] ";
			match superClassOpt with
			| None -> ()
			| Sum s -> Printf.printf "[token: colon] [token: classname: %s] " s;
			Printf.printf "[token: is]\n";
			printBlocType bloc;
			Printf.printf "[token: rbrace]\n]"
		)
	| SimpleMethod (isStatic, isOverridden, name, lparam, classname, e) ->
		(
			Printf.printf "[";
			printMethodWithoutInstr isStatic isOverridden name lparam classname;
			printExpr e;
			Printf.printf "[token: semi-colon]]\n"
		)
	| ComplexMethod (isStatic, isOverridden, name, lparam, classname, block) -> 
		(
			Printf.printf "[";
			printMethodWithoutInstr isStatic isOverridden name lparam classname;
			printBlocType block;
			Printf.printf "[token: semi-colon]]\n"
		)
;;

let printDecl c =
	Printf.printf "[[token: class] [token: classname: %s] " c.classname;
	printConstrLParam c.lparam;
	match c.superClassOpt with
	| None -> ()
	| Sum s -> Printf.printf "[token: extends] [token: classname: %s] " s;
	Printf.printf "[token: is]\n[token: lbrace]\n";
	List.iter (fun e -> printClassElem e) c.ce;
	Printf.printf "]\n"
;;

let printProg p =
	Printf.printf "[\n";
	List.iter printDecl p.classes;
	Printf.printf "\n[token: lbrace]`\n";
	List.iter printInstr p.instrs;
	Printf.printf "[token: rbrace]\n]\n"
;;
