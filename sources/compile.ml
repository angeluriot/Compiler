open Ast

(* Les "références" sont utilisées pour simuler des variables "globales" dans
 * lesquelles on fera des affectation. Quand on declare une référence, on doit
 * donner sa valeur initiale pour le compilateur ocaml puisse inférer le type
 * de valeurs qu'elle va référencer
 * Exemple d'usage
 * let cpt = ref 0    cpt est une "référence" sur un entier et vaut 0
 * ! cpt              accède à la valeur courante de la reference cpt
 * cpt := 23          change la valeur de la reference cpt.
 *
 * Ci-dessous on s'en sert pour avoir un compteur qui va permettre de générer
 * des noms d'étiquette différents à chaque appel.
 * Correspond à ce qu'on ferait en Java avec un compteur qui serait un champ
 * de classe déclaré comme
 * static int cpt
 *)
let cptEti = ref 0	(* compteur: variable globale *)

(* generateur d'etiquettes fraiches. A chaque appel, on renvoie une paire
 * d'étiquettes dont on garantit qu'elles n'ont jamais été utilisées.
 * Sert ici à produire des étiquettes pour engendrer du code pour les
 * if-then-else, comme vu en TD.
 * Les étiquettes auront la forme "else1", "fin1", "else2", "fin2", etc
 *)
let makeEti () =
	let v = ! cptEti in
	let sv = string_of_int v in
	cptEti := v + 1;
	("else"^sv, "fin"^sv) (* ^ est l'operateur de concatenation de strings *)


(* paramètres: la liste de déclarations, l'expression finale et le fichier
 * dans lequel on écrit le code engendré par notre compilateur
 *)
let compile p chan =
	let compileClasses ld chan = ()
	in
	let compileExpr e env = match e with
		Id x ->
			begin
				(* retrouve le rang, donc l'adresse par rapport a GP, de la variable *)
				try let adr = List.assoc x env
				in output_string chan "PUSHG ";
				output_string chan (string_of_int adr);
				output_string chan "\n";
				with Not_found ->
				(* ne peut arriver si les vérifications contextuelles sont faites *)
				failwith "unexpected situation in compileExpr"
			end
		| Cste v ->
			output_string chan "PUSHI "; output_string chan (string_of_int v);
			output_string chan "\n";
		| Plus(g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "ADD\n";
		| Minus (g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "SUB\n";
		| Times (g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "MUL\n";
		| Div (g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "DIV\n";
		| UMinus e -> (* traduit en 0 - e *)
			output_string chan "PUSHI 0\n";
			compileExpr e env;
			output_string chan "SUB\n"
		| Ite (si, alors, sinon) ->
			let (etiElse, etiFin) = makeEti () in
			compileExpr si env;
			output_string chan "JZ "; output_string chan etiElse;
			output_string chan "\n";
			compileExpr alors env;
			output_string chan "JUMP "; output_string chan etiFin;
			output_string chan "\n";
			output_string chan etiElse; output_string chan ": NOP\n";
			compileExpr sinon env;
			output_string chan etiFin; output_string chan ": NOP\n";
		| Comp(op, g, d) ->
			(* On montre l'autre facon de faire, par rapport a eval: on traite les
			 * operateurs de comparaison ici, meme s'ils ne peuvent apparaitre
			 * que dans la condtion d'un IF. Pour eval on avait un probleme
			 * car l'évaluation d'une expression arithmétique donne un int alors
			 * que l'expression d'une comparaison donne un booléen en ocaml.
			 *)
			compileExpr g env; compileExpr d env;
			match op with
			Eq ->   output_string chan "EQUAL\n"
			| Neq ->  output_string chan "EQUAL\nNOT\n"
			| Lt ->   output_string chan "INF\n"
			| Le ->   output_string chan "INFEQ\n"
			| Gt ->   output_string chan "SUP\n"
			| Ge ->   output_string chan "SUPEQ\n"

	output_string chan "START\n";
	compileExpr e (compileDecl ld [] 0);

	output_string chan
	"PUSHS \"Resultat: \"\nWRITES\nWRITEI\nPUSHS \"\\n\"\nWRITES\nSTOP\n";
	flush chan;
	close_out chan;

	(*
		let rec compileDecl ld env rang =
		(* compile chaque partie droite de declaration et associe à sa partie
			gauche son rang dans la liste des declarations, qui correspond au
			decalage de son emplacement par rapport à GP.
			rang: rang pour la prochaine variable (initialisee a 0 au premier appel)
			env :liste des couples (variable, rang de la variable ) deja traites
			ld : liste des declarations a traiter
			Avec fold_left, il aurait fallu que l'accumulateur contienne une paire
			contenant l'environnement courant et le rang.
			Ici on utilise une fonction récursive traditionnelle
		*)
		match ld with
		[] -> env
		| { lhs; rhs; } :: ld' ->
			compileExpr rhs env;
			compileDecl ld' ((lhs, rang)  :: env) (rang+1)
	and compileExpr e env =
		match e with
		Id x ->
			begin
				(* retrouve le rang, donc l'adresse par rapport a GP, de la variable *)
				try let adr = List.assoc x env
				in output_string chan "PUSHG ";
				output_string chan (string_of_int adr);
				output_string chan "\n";
				with Not_found ->
				(* ne peut arriver si les vérifications  tuelles sont faites *)
				failwith "unexpected situation in compileExpr"
			end
		| Cste v ->
			output_string chan "PUSHI "; output_string chan (string_of_int v);
			output_string chan "\n";
		| Plus(g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "ADD\n";
		| Minus (g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "SUB\n";
		| Times (g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "MUL\n";
		| Div (g, d) ->
			compileExpr g env; compileExpr d env; output_string chan "DIV\n";
		| UMinus e -> (* traduit en 0 - e *)
			output_string chan "PUSHI 0\n";
			compileExpr e env;
			output_string chan "SUB\n"
		| Ite (si, alors, sinon) ->
			let (etiElse, etiFin) = makeEti () in
			compileExpr si env;
			output_string chan "JZ "; output_string chan etiElse;
			output_string chan "\n";
			compileExpr alors env;
			output_string chan "JUMP "; output_string chan etiFin;
			output_string chan "\n";
			output_string chan etiElse; output_string chan ": NOP\n";
			compileExpr sinon env;
			output_string chan etiFin; output_string chan ": NOP\n";
		| Comp(op, g, d) ->
			(* On montre l'autre facon de faire, par rapport a eval: on traite les
			 * operateurs de comparaison ici, meme s'ils ne peuvent apparaitre
			 * que dans la condtion d'un IF. Pour eval on avait un probleme
			 * car l'évaluation d'une expression arithmétique donne un int alors
			 * que l'expression d'une comparaison donne un booléen en ocaml.
			 *)
			compileExpr g env; compileExpr d env;
			match op with
			Eq ->   output_string chan "EQUAL\n"
			| Neq ->  output_string chan "EQUAL\nNOT\n"
			| Lt ->   output_string chan "INF\n"
			| Le ->   output_string chan "INFEQ\n"
			| Gt ->   output_string chan "SUP\n"
			| Ge ->   output_string chan "SUPEQ\n"
	in
	*)