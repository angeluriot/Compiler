open Ast

(* todo changer les true false en error *)
let vc_defined_classes p =
	let rec ok_expr e env = match e with
		| Cast (s, _) -> if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s))
		| Instantiation (s, _) -> if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s))
		| StaticFieldAccess (s, _) -> if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s))
		| StaticMethodCall (s, _, _) -> if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s))
		| _ -> ()
	in
	let ok_constr_params lparam env =
		List.iter (fun param -> if not (List.mem param.classname_constr_param env) then raise (VC_Error ("Classe non déclarée : " ^ param.classname_constr_param))) lparam
	in
	let rec ok_instr i env = match i with
		| Expr e -> ok_expr e env
		| Return -> ()
		| Assignment(lhs, rhs) -> ok_expr lhs env; ok_expr rhs env
		| Ite(i, t, e) -> ok_expr i env; ok_instr t env; ok_instr e env
		| BlockInstr b -> ok_block b env
	and ok_block block env = match block with
		| Block li -> List.iter (fun i -> ok_instr i env) li
		| BlockVar (lparam, li) ->
			List.iter (
			fun param -> if not (List.mem param.classname_method_param env) then raise (VC_Error ("Classe non déclarée : " ^ param.classname_method_param))
		) lparam;
			List.iter (fun i -> ok_instr i env) li
	in
	let ok_superclass sc env = match sc with
		| None -> ()
		| Some s -> if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s))
	in
	let ok_superclass_constructor sc env = match sc with
		| None -> ()
		| Some (s, lexpr) ->
			if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s));
			List.iter (fun e -> ok_expr e env) lexpr
	in
	let ok_classelem env ce = match ce with
		| Field (_, _, s) -> if not (List.mem s env) then raise (VC_Error ("Classe non déclarée : " ^ s))		
		| Constr(classname, lparam, superClassOpt, b) ->
			if not (List.mem classname env) then raise (VC_Error ("Classe non déclarée : " ^ classname));
			ok_constr_params lparam env;
			ok_superclass_constructor superClassOpt env;
			ok_block b env
		| SimpleMethod(_, _, _, lparam, classname, e) ->
			ok_constr_params lparam env;
			if not (List.mem classname env) then raise (VC_Error ("Classe non déclarée : " ^ classname));
			ok_expr e env
		| ComplexMethod(_, _, _, lparam, superClassOpt, b) ->
			ok_constr_params lparam env;
			ok_superclass superClassOpt env;
			ok_block b env
	in
	let rec ok_classes classes env = match classes with
		| [] -> env
		| c :: s -> (
			let env_s = if List.mem c.classname env then env else c.classname::env
			in
			ok_constr_params c.lparam env_s;
			ok_superclass c.superClassOpt env_s;
			List.iter (ok_classelem env_s) c.ce;
			ok_classes s env_s)
	in
	let env_classes = ok_classes p.classes (["Integer"; "String"])
	in
	ok_block p.block env_classes;
	env_classes
;;

let vc_redeclaration_class ld =
	List.fold_left (
		fun acc d ->
			if List.mem d.classname acc
			then raise (VC_Error ("Redéfinition de " ^ d.classname))
			else d.classname :: acc
	) (["Integer"; "String"]) ld;
	()
;;

let vc_available_methods_fields p = ()
;;

let vc_inheritance_cycle ld =
	let get_class_inheritance_info ld =
		let extract_classname d =
			match d.superClassOpt with
			| None -> (d.classname, "")
			| Some s -> (d.classname, s)
		in
		List.fold_left (fun acc x -> extract_classname x :: acc) [] ld
	in
	let find_superclass c l =
		List.fold_left (
			fun acc x ->
				let (c2, _) = x
				in
				if c = c2 then x else acc 
		) ("", "") l
	in
	let rec check_cycle c l seen =
		let (classname, superclassname) = c
		in
		if superclassname = "" then ()
		else
			if List.mem classname seen
			then raise (VC_Error ("Cycle detected with " ^ classname))
			else check_cycle (find_superclass superclassname l) l (classname::seen)
	in
	let classes = get_class_inheritance_info ld
	in
	List.iter (fun c -> check_cycle c classes []) classes
;;


(* verifie si l'expression e ne reference bien que des variables qui figurent
 * dans la liste de variables lvars.
 * Leve l'exception VC_Error si une variable n'a pas été déclarée, sinon
 * retourne () en résultat.
 *)

 (*
 
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
*)

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
let vc p =
	vc_redeclaration_class p.classes;
	vc_inheritance_cycle p.classes;
	let classnames = vc_defined_classes p
	in
	()

;;

	(* Construit progressivement grace a List.fold_left la liste des
	 * variables deja rencontrées tout en procédant aux vérifications.
	 * On peut aussi faire cela avec une fonction récursive si on ne veut pas
	 * recourir à fold_left
	 *)
	 (*

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

				(* renvoie une liste avec la nouvelle variable ajoutée à la liste des
				 * variables connues. L'ordre des variables dans la liste n'important
				 * pas ici, on la met en tête puisque c'est plus pratique
				 *)
				lhs::lvars
			) (* fin de la fonction qui est le premier argument de fold_left *)
			[] (* valeur initiale de l'accumulateur *)
			ld (* liste parcourue par fold_left *)
	in
	(* on a recupéré la liste de toutes les variables déclarées, il ne reste
	 * plus qu'à vérifier l'expression finale
	 *)
	vc_expr e allVars
	 *)

let eval p = 0
	(* evalDecl: prend une liste de declarations et renvoie une liste
	 * (variable, valeur) qui associe à chaque variable le résultat de
	 * l'evaluation de l'expression en partie droite de la déclaration.
	 *
	 * ld : la liste des declarations a traiter
	 * env : la liste d'association résultant des declarations deja traitees.
	 *
	 * On aurait pu de nouveau utiliser List.fold_left. On montre ici uen
	 * version avec une fonction récursive qui parcourt la liste à la main.
	 *)

	 (*
	 
	let rec evalDecl ld env =
		match ld with
			(* On a traité toutes les déclarations => on renvoie l'environnement *)
			[] -> env
		| (* Evalue la partie droite dans l'environnement courant et ajoute le
			 * nouveau couple (variable, valeur) à l'environnement transmis pour
			 * l'appel suivant.
			 *)
			{ lhs; rhs; } :: ld' ->
				evalDecl ld' ((lhs, evalExpr rhs env) :: env)
	and evalComp condition env =
		match condition with
		Comp(op, g, d) ->
			let vg = evalExpr g env and vd = evalExpr d env in
			begin
				match op with
				Eq ->  vg = vd
				| Neq -> vg != vd
				| Lt ->  vg < vd
				| Le ->  vg <= vd
				| Gt ->  vg > vd
				| Ge ->  vg >= vd
			end
		| _ ->
			 (* une comparaison est forcement de la forme Comp(...). Si on trouve
				* autre chose c'est qu'il y a un pb: AST mal construit
				*)
			 failwith "unexpected situation in evalComp"
	and evalExpr e env =
		match e with
			Id x ->
			(* L'exception ne peut pas arriver si les vérifications contextuelles
			 * sont correctes.
			 *)
			begin
				(* begin end nécessaire pour que les autres cas du match ne soient
				 * pas considérées comme des noms d'exceptions gérées par le
				 * try ... with qui accepte en général plusieurs exceptions
				 *)
				try List.assoc x env
				with Not_found -> failwith ("Unexpected situation in evalExpr")
			end
		| Cste v -> v
		| Plus(g, d) -> (evalExpr g env) + (evalExpr d env)
		| Minus (g, d) -> (evalExpr g env) - (evalExpr d env)
		| Times (g, d) -> (evalExpr g env) * (evalExpr d env)
		| Div (g, d) ->
			let vg = (evalExpr g env) and vd = (evalExpr d env) in
			if vd = 0 then
				raise (RUN_Error "division par 0")
			else vg / vd
		| UMinus e -> - (evalExpr e env)
		| Ite (si, alors, sinon) ->
			if evalComp si env then (evalExpr alors env)
			else (evalExpr sinon env)
		| Comp _ ->
			 (* une expression de comparaison ne peut pas apparaitre ailleurs que
			 * dans un IF et elle sera alors traitée par evalComp. Si on la voit
			 * ici c'est qu'il y a un probleme.
			 *)
			failwith "unexpected situation in evalExpr"
	in let final_env = evalDecl ld [] in (* traite les déclarations *)
		evalExpr e final_env (* traite l'expression finale *)

		*)
;;
