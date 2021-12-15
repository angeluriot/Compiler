{
	open Ast
	open Parse
	open Lexing
	exception Eof

	(* gere les positions numero de ligne + decalage dans la ligne *)
	let next_line lexbuf = Lexing.new_line lexbuf

	(* Potentiellement utile pour distinguer mots-clef et vrais identificateurs *)
	let keyword_table = Hashtbl.create 16

	let _ =
		List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
		[
			"begin", BEGIN;
			"end", END;
			"if", IF;
			"then", THEN;
			"else", ELSE;
		]
}

(* abréviation utiles pour les expressions rationnelles *)
let lettre = ['A'-'Z' 'a'-'z']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )


(* l'analyseur lexical est decomposé ici en deux fonctions: l'une qui est
 * specialisée dans la reconnaissance des commentaires à la C, l'autre qui
 * traite les autres tokens à reconnaire.
 * Tout caractere lu dans le fichier doit être reconnu quelque part ! *)
rule
	token = parse
	lettre LC * as id		{
								try
									Hashtbl.find keyword_table id
								with
									Not_found -> ID id
							}
	| chiffre * as cste		{ CSTE (int_of_string cste) }
	| '('					{ LPAREN }
	| ')'					{ RPAREN }
	| ';'					{ SEMICOLON }
	| ":="					{ ASSIGN }
	| '+'					{ PLUS }
	| '-'					{ MINUS }
	| '*'					{ TIMES }
	| '/'					{ DIV }
	| '>'					{ RELOP Ast.Gt }
	| ">="					{ RELOP Ast.Ge }
	| '='					{ RELOP Ast.Eq }
	| "<="					{ RELOP Ast.Le }
	| '<'					{ RELOP Ast.Lt }
	| "<>"					{ RELOP Ast.Neq }
	| [' ''\t''\r']			{ token lexbuf }
	| '\n'					{ next_line lexbuf; token lexbuf }
	| "/*"					{ comment lexbuf }
	| eof					{ EOF }
	| _ as lxm				{
								print_endline
								("undefined character: " ^ (String.make 1 lxm));
								token lexbuf
							}
and
	comment = parse
	"*/"				{ token lexbuf }
	| '\n'				{ next_line lexbuf; comment lexbuf }
	| _					{ comment lexbuf }
