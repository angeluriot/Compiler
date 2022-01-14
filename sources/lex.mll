{
	open Ast
	open Parse
	open Lexing
	exception Eof

	(* gere les positions numero de ligne + decalage dans la ligne *)
	let next_line lexbuf = Lexing.new_line lexbuf

	(* cree une table de hachage qu'on remplit avec le token associe a chaque mot-clef *)
	let keyword_table = Hashtbl.create 16
	let _ =
	List.iter
		(fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
		[
			"class", CLASS;
			"is", IS;
			"var", VAR;
			"def", DEF;
			"static", STATIC;
			"extends", EXTENDS;
			"override", OVERRIDE;
			"this", THIS;
			"super", SUPER;
			"result", RESULT;
			"new", NEW;
			"if", IF;
			"then", THEN;
			"else", ELSE
		]
}

let lettre = ['a'-'z']
let lettreMaj = ['A'-'Z']
let chiffre = ['0'-'9']

let LC = ( chiffre | lettre | lettreMaj )
let LDebut = ( lettre | lettreMaj )

rule
	comment = parse
		"*/"	{
					(* fin de commentaire trouvee. Le commentaire ne doit pas
					 * etre transmis. On renvoie donc ce que nous renverra un
					 * nouvel appel a l'analyseur lexical *)
					token lexbuf
				}
		| '\n'	{
					(* incremente le compteur de ligne et poursuit la
					 * reconnaissance du commentaire en cours *)
					new_line lexbuf; comment lexbuf
				}
		| eof	{
					(* detecte les commentaires non fermes pour pouvoir
					 * faire un message d'erreur clair.
					 * On pourrait stocker la position du dernier commentaire
					 * encore ouvert pour ameliorer le dioagnostic *)
					raise (MISC_Error "unclosed comment")
				}
		| _		{
					(* rien a faire de special pour ce caractere, donc on
					 * poursuit la reconnaissance du commentaire en cours *)
					comment lexbuf
				}
	and
	str buf = parse
		'"'				{ STRING (Buffer.contents buf) }
			| '\n'		{
							(* incremente le compteur de ligne et poursuit la
							 * reconnaissance du commentaire en cours *)
							Buffer.add_char buf '\n'; new_line lexbuf; str buf lexbuf
						}
		| '\\' '/'		{ Buffer.add_char buf '/'; str buf lexbuf }
		| '\\' '\\'		{ Buffer.add_char buf '\\'; str buf lexbuf }
		| '\\' 'b'		{ Buffer.add_char buf '\b'; str buf lexbuf }
		| '\\' 'f'		{ Buffer.add_char buf '\012'; str buf lexbuf }
		| '\\' 'n'		{ Buffer.add_char buf '\n'; str buf lexbuf }
		| '\\' 'r'		{ Buffer.add_char buf '\r'; str buf lexbuf }
		| '\\' 't'		{ Buffer.add_char buf '\t'; str buf lexbuf }
		| '\\' '"'		{ Buffer.add_char buf '"'; str buf lexbuf }
		| [^ '"' '\\']+	{
							Buffer.add_string buf (Lexing.lexeme lexbuf);
							str buf lexbuf
						}
		| eof			{
							(* detecte les commentaires non fermes pour pouvoir
							 * faire un message d'erreur clair.
							 * On pourrait stocker la position du dernier commentaire
							 * encore ouvert pour ameliorer le dioagnostic *)
							raise (MISC_Error "String is not terminated")
						}
	and
	token = parse
		| lettreMaj LC * as classname
		{
			CLASSNAME classname
		}

		| lettre LDebut * as id
		{
			(* id contient le texte reconnu. On verifie s'il s'agit d'un mot-clef
			 * auquel cas on renvoie le token associe. Sinon on renvoie Id avec le
			 * texte reconnu en valeur *)
			try
				Hashtbl.find keyword_table id
			with Not_found -> ID id
		}

		| lettre LC * as id
		{
			(* id contient le texte reconnu. On verifie s'il s'agit d'un mot-clef
			 * auquel cas on renvoie le token associe. Sinon on renvoie Id avec le
			 * texte reconnu en valeur *)
			try
				Hashtbl.find keyword_table id
			with Not_found -> ID id
		}

		| [' ''\t''\r']+	{
								(* consommer les delimiteurs, ne pas les transmettre
								 * et renvoyer ce que renverra un nouvel appel a
								 * l'analyseur lexical *)
								token lexbuf
							}
		| '\n'				{ next_line lexbuf; token lexbuf}
		| chiffre+ as lxm	{ CSTE(int_of_string lxm) }
		| "/*"				{ comment lexbuf }
		| '+'				{ PLUS }
		| '-'				{ MINUS }
		| '*'				{ TIMES }
		| '/'				{ DIV }
		| '('				{ LPAREN }
		| ')'				{ RPAREN }
		| '{'				{ LBRACE }
		| '}'				{ RBRACE }
		| ':'				{ COLON }
		| ';'				{ SEMICOLON }
		| '&'				{ CONCAT }
		| ":="				{ ASSIGN }
		| "<"				{ RELOP (Ast.Lt) }
		| "<="				{ RELOP (Ast.Le) }
		| ">"				{ RELOP (Ast.Gt) }
		| ">="				{ RELOP (Ast.Ge) }
		| "="				{ RELOP (Ast.Eq) }
		| "<>"				{ RELOP (Ast.Neq) }
		| '"'				{ str (Buffer.create 20) lexbuf }
		| ','				{ COMMA }
		| '.'				{ DOT }
		| eof				{ EOF }
		| _ as lxm			{
								(* action par défaut: filtre un unique caractere, different
								 * de ceux qui precedent. Il s'agit d'un caratere errone:
								 * on le signale et on poursuit quand meme l'analyse.
								 * On aurait pu décider de lever une exception et
								 * arreter l'analyse. *)
								print_endline
								("undefined character: " ^ (String.make 1 lxm));
								token lexbuf
							}
