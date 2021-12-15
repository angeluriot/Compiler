open Lexing
open Ast
open Parse

let output token =
		match token with
		CSTE v		-> "Constante entiere: " ^ (string_of_int v)
		| ID id		-> "Ident: " ^ id
		| RELOP op	-> "operateur " ^ Utils.string_of_relop op
		| PLUS		-> "operateur +"
		| MINUS		-> "operateur -"
		| TIMES		-> "operateur *"
		| DIV		-> "operateur /"
		| ASSIGN	-> "operateur :="
		| SEMICOLON	-> "symbole ;"
		| LPAREN	-> "symbole ("
		| RPAREN	-> "symbole )"
		| END		-> "mot-clef: END"
		| BEGIN		-> "mot-clef: BEGIN"
		| IF		-> "Mot-clef: IF"
		| THEN		-> "Mot-clef: THEN"
		| ELSE		-> "Mot-clef: ELSE"
		| EOF		-> failwith "Should not happen in testLex"

(* usage: ./testLex nom-de-fichier
 * Applique l'analyseur lexical sur le fichier et imprime les tokens reconnus
 * (sauf ceux non transmis comme les delimiteurs et les commentaires)
 *)
let () =
	if Array.length Sys.argv = 1
	then print_endline "usage: textLex nom-de-fichier"
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
		end;;
