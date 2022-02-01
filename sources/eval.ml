open Ast

let rec does_method_have_a_result ce =
	match ce with
	| Field | Constr -> ()
	| SimpleMethod (b1, b2, s1, cl, s2, e) -> does_expr_have_a_result e
	| ComplexMethod (b1, b2, s1, cl, s2, bt) -> does_blocktype_have_a_result bt

let rec is_this_or_super_in_block p =
	let is_expr_this_or_super e =
		match e with
			| This -> true
			| Super -> true
			| _ -> false
	in
	let rec instr_this_or_super l2 = match l2 with
		| Return | BlockInstr -> false	
		| Assignment (e1, e2) -> is_expr_this_or_super e1 || is_expr_this_or_super e2
		| Ite (e, i1, i2) -> is_expr_this_or_super e 
	in
	let rec is_this_or_super_in_block_sub l = match l with
		| [] -> false
		| x::s -> is_this_or_super_in_block_sub s || instr_this_or_super x
	in
	match p.block with
		| Block(l) -> if is_this_or_super_in_block_sub l then raise (VC_Error ("This or Super in block"))
		| BlockVar(ml, l) -> if is_this_or_super_in_block_sub l then raise (VC_Error ("This or Super in block"))

(* verifie si l'expression e ne reference bien que des variables qui figurent
 * dans la liste de variables lvars.
 * Leve l'exception VC_Error si une variable n'a pas été déclarée, sinon
 * retourne () en résultat.
 *)
let vc_expr e lvars =
	let rec vc_e e = (* fonction auxiliaire qui parcourt récursivement e *)
		match e with
			Id x ->
				if not (List.mem x lvars) then
					raise (VC_Error ("variable non declaree: " ^ x))
			| Cste v -> ()
			| Plus(g, d) | Minus (g, d) | Times (g, d) | Div (g, d) ->
				vc_e g; vc_e d;
			| UMinus e -> vc_e e
			| Ite (si, alors, sinon) ->
				vc_e si; vc_e alors; vc_e sinon;
			| Comp(op, g, d) -> vc_e g; vc_e d;
	in vc_e e


(* lance les vérifications contextuelles sur la liste de déclarations ainsi
 * que l'expression finale. D'après l'énoncé il s'agit ici de vérifier que
 * les expressions ne référencent que des variables déclarées et qu'une variable
 * n'est déclarée qu'une fois. Pour cela on va simplement construire une
 * liste des variables déjà déclarées. Ici on n'a pas besoin de connaitre la
 * valeur de ces variables, juste leur existence.
 * L'énoncé demande que cette vérification soit faite avant l'exécution et qu'on
 * reporte le fait qu'une variable ne soit pas déclarée indépendamment du fait
 * qu'on ait besoin ou pas de sa valeur à l'exécution.
 *)
let vc ld e =
	(* Construit progressivement grace a List.fold_left la liste des
	 * variables deja rencontrées tout en procédant aux vérifications.
	 * On peut aussi faire cela avec une fonction récursive si on ne veut pas
	 * recourir à fold_left
	 *)
	let allVars =
		List.fold_left (* voir la doc de fold_left pour le rôle des 3 arguments *)
			(fun lvars decl ->
				(* prend en paramètre l'accumulateur, ie. la liste des variables déjà
				 * déclarées (initialement [], le 2eme argument de fold_left) et la
				 * déclaration à traiter.
				 *
				 * { lhs; rhs; } est un raccourci pour { lhs = lhs; rhs = rhs }
				 * Les noms de champ jouent le rôle de variables locales dans la
				 * decomposition du record.
				 *)
				let { lhs; rhs } = decl in
				vc_expr rhs lvars; (* verifier la partie droite de la déclaration *)

				(* vérifier que lhs n'a pas dejà été déclarée *)
				if List.mem lhs lvars then
					raise (VC_Error ("redeclaration de la variable " ^ lhs));



let rec vc_integer_string_final ld =
	List.iter (fun d -> 
		if d.classname = "Integer" || d.classname = "String" then raise (VC_Error ("La classe "^d.classname^" ne peut pas être redéfinie."));
		match d.superClassOpt with
			| Some s -> if s = "Integer" || s = "String" then raise (VC_Error ("La classe "^s^" ne doit pas avoir de sous-classe."))
			| _ -> ()
	) ld
;;

let checkOneConstructor d = 
	let cpt = List.fold_left (fun acc x ->
		match x with
		| Constr (_,_,_,_) -> acc +1
		| _ -> acc
	) 0 d.ce
	in
	if cpt = 0 then raise (VC_Error ("La classe "^d.classname^" n'a pas de constructeur.")) else 
		if cpt != 1 then raise (VC_Error ("La classe "^d.classname^" a plus d'un constructeur."))
;;
