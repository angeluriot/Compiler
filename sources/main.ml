open Ast
open Lexing
open Eval

(* lexbuf: correspond au buffer d'entrée associé au programme qu'on traite
 * file_in: descripteur de fichier pour ce programme
 * chan: descripteur de fichier dans lequel on ecrit le code produit pour la
 * partie compilation. Par défaut le code ira dans le fichier out.txt
 *)
let parse_with_error lexbuf file_in chan =
	let print_position outx lexbuf =
		let pos = lexbuf.lex_curr_p in
		Printf.fprintf outx "%s:%d:%d" file_in
			pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
	in
	try
		(* lance l'analyse syntaxique (qui appellera l'analyse lexicale) en
		 * executant au fur et à mesure les actions associées aux productions.
		 * L'appel est fait par Parse.prog (où prog est le non-terminal déclaré
		 * comme axiome dans parse.mly).
		 * La valeur retournée par la production qui définit l'axiome renvoie un
		 * couple forme par la représentation de la partie déclarations ainsi que
		 * la représentation de l'expression comprise entre le begin et le end.
		 * Cette structure correspond au type progType défini dans ast.ml.
		 * Ci-dessous ld contient donc une liste de records dont chacun représente
		 * une déclaration et l'ast de l'expression comprise entre begin et end
		 *)
		let p = Parse.program Lex.token lexbuf in

		(* Dans ce TP d'initiation on réalise à la fois l'impression des AST,
		 * les vérifications contextuelles, une version sous forme d'interprète
		 * et une version sous forme d'un compilateur pour la machine virtuelle
		 * utilisee pour le projet.
		 *)

		(*
			Print.printProg p; 
		 *)
		
		(* impression non ambigue de tout l'AST *)

		(* Verifications Contextuelles: incluses dans le fichier eval.ml
		 * Lance l'exception VC_Error en cas d'erreur. En ce cas ni la partie
		 * interprétation, ni la partie compilation ne sera lancée et on se
		 * retrouvera directement dans le traite-exception ci-dessous.
		 *)

		(*		
		List.iter (fun (a, b) -> Printf.printf "(%s, %s)\n" a b) (Eval.get_class_inheritance_info p.classes);

		let methods = Eval.get_classes_methods_list p.classes
		in
		List.iter (fun (classname, coef, name, lparam, returnedClass) ->
			Printf.printf "(%s, %i, %s, %s, [" classname coef name returnedClass;
			List.iter (fun p -> let (name, type_) = p in
				Printf.printf "(%s, %s), " name type_
			) lparam;
			Printf.printf "])\n"
		) methods
		;
		Eval.vc_surcharge p.classes;
		*)

		Eval.vc p;


		(* partie compilation: on engendre du code pour la machine abstraite *)
		(*Compile.compile p chan;*)
	
	
	with (* traite exception général ... *)
		Parse.Error -> (* levée par l'analyseur syntaxique *)
		Printf.fprintf stderr "Syntax error at position %a\n" print_position lexbuf;
		exit (-1)
	| VC_Error msg ->
		Printf.fprintf stderr "Erreur contextuelle: %s\n" msg;
		exit (-1)
	| RUN_Error msg -> (* uniquement pour la version interprete *)
		Printf.fprintf stderr "Erreur à l'execution: %s\n" msg;
		exit (-1)
	| MISC_Error msg -> (* pour l'instant juste erreur lexicale *)
		Printf.fprintf stderr "Error: %s\n" msg;
		exit (-1)

let _ =
	let argc = Array.length Sys.argv in
	if argc = 1 then
		print_endline "usage: tp programme [fichier-pour-le-code] "
	else
		begin
			(* si on ne passe pas à l'appel le nom du fichier dans lequel
			 * ecrire le code produit, on utilise par défaut le fichier "out.txt"
			 *)
			let file_out = if argc = 3 then Sys.argv.(2) else "out.txt"
			and file_in = Sys.argv.(1) in
			let chan_in = open_in file_in
			and chan_out = open_out file_out in
			let lexbuf = Lexing.from_channel chan_in
			in
			parse_with_error lexbuf file_in chan_out;
			close_in chan_in;
			close_out chan_out
		end