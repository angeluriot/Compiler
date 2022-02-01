open Lexing
open Ast
open Parse

let output token =
		match token with
		CSTE v		-> "Constante entiere: " ^ (string_of_int v)
		| ID id		-> "Idend: " ^ id
		| RELOP op	-> "operateur " ^ Utils.string_of_relop op
		| PLUS		-> "operateur +"
		| MINUS		-> "operateur -"
		| TIMES		-> "operateur *"
		| DIV		-> "operateur /"
		| SEMICOLON	-> "symbole ;"
		| LPAREN	-> "symbole ("
		| RPAREN	-> "symbole )"
		| ASSIGN	-> "symbole d'affectation"
		| IF		-> "mot-clef: IF"
		| THEN		-> "mot-clef: THEN"
		| ELSE		-> "mot-clef: ELSE"
		| END		-> "mot-clef: END"
		| BEGIN		-> "mot-clef: BEGIN"
		| EOF		-> (* gere avant l'appel a cette fonction, donc impossible *)
			failwith "Should not happen in testLex"
		| UNIT ->
			failwith "UNIT seen in testLex"
		| _ -> "Unexpected token in testLex"

(* usage: ./testLex nom-de-fichier
 * Applique l'analyseur lexical sur le fichier et imprime les tokens reconnus
 * (sauf ceux non transmis comme les delimiteurs et les commentaires)
 *)
let () =
	if Array.length Sys.argv = 1 then
		print_endline "usage: textLex nom-de-fichier"
	else
		begin
			let file = open_in Sys.argv.(1) in
			let lexbuf = Lexing.from_channel file
			in
			let rec process () =
				match Lex.token lexbuf with
					EOF -> close_in file
				| tok -> print_endline (output tok); process ()
			in process ();
		end
;;