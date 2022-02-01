open Ast

let get_classes_names ld = List.fold_left (fun acc x -> x.classname :: acc) [] ld
;;

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
	let ok_method_params lparam env =
		List.iter (fun param -> if not (List.mem param.classname_method_param env) then raise (VC_Error ("Classe non déclarée : " ^ param.classname_method_param))) lparam
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
			ok_method_params lparam env;
			if not (List.mem classname env) then raise (VC_Error ("Classe non déclarée : " ^ classname));
			ok_expr e env
		| ComplexMethod(_, _, _, lparam, superClassOpt, b) ->
			ok_method_params lparam env;
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
	let env_classes = ok_classes p.classes (get_classes_names p.classes @ ["Integer"; "String"])
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
	) (["Integer"; "String"]) ld
;;

(* // TODO *)
let vc_available_methods_fields p = ()
;;

let get_class_inheritance_info ld =
	let extract_classname d =
		match d.superClassOpt with
		| None -> (d.classname, "")
		| Some s -> (d.classname, s)
	in
	List.fold_left (fun acc x -> extract_classname x :: acc) [] ld
;;

let vc_inheritance_cycle ld =
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

let vc_portee_id p =
	let contains id env = List.fold_left (fun acc (x, _) -> acc || x = id) false env
	in
	let ajoute_params p type_ env = env @ [(p, type_)]
	in
	let ajoute_constr_param_list lparam env =
		if (lparam.var) then env else
		List.fold_left (fun acc lp -> ajoute_params lp lparam.classname_constr_param acc) env lparam.param
	in
	let ajoute_method_param_list lparam env =
		List.fold_left (fun acc lp -> ajoute_params lp lparam.classname_method_param acc) env lparam.param
	in
	let ajoute_method_params_list lm env =
		List.fold_left (fun acc x -> ajoute_method_param_list x acc) env lm
	in
	let ajoute_constr_params_list lm env =
		List.fold_left (fun acc x -> ajoute_constr_param_list x acc) env lm
	in
	let rec portee_expr e env = match e with
		| Id x -> if contains x env then () else raise (VC_Error ("Id non déclaré : " ^ x))
		| Cast (_, e) -> portee_expr e env
		| Instantiation (_, le) -> List.iter (fun e -> portee_expr e env) le
		| FieldAccess (e, _) -> portee_expr e env
		| MethodCall (e, _, le) -> portee_expr e env; List.iter (fun e -> portee_expr e env) le
		|	StaticMethodCall (_, _, le) -> List.iter (fun e -> portee_expr e env) le
		| Comp (_, lhs, rhs) -> portee_expr lhs env; portee_expr rhs env
		| Concat (lhs, rhs) -> portee_expr lhs env; portee_expr rhs env
		| Plus (lhs, rhs) -> portee_expr lhs env; portee_expr rhs env
		| Minus (lhs, rhs) -> portee_expr lhs env; portee_expr rhs env
		| Times (lhs, rhs) -> portee_expr lhs env; portee_expr rhs env
		| Div (lhs, rhs) -> portee_expr lhs env; portee_expr rhs env
		| UMinus e -> portee_expr e env
		| _ -> ()
	in
	let rec portee_instr i env = match i with
		| Expr e -> portee_expr e env
		| Assignment (lhs, rhs) ->
			portee_expr lhs env;
			portee_expr rhs env
		| Ite (i, t, e) ->
			portee_expr i env;
			portee_instr t env;
			portee_instr e env
		| BlockInstr b -> portee_bloc b env
		| _ -> ()
	and
	portee_bloc b env = match b with
		| Block(li) -> List.iter (fun i -> portee_instr i env) li
		| BlockVar(lm, li) -> List.iter (fun i -> portee_instr i (ajoute_method_params_list lm env)) li
	in
	let portee_decl d env =
		List.iter (fun ce -> match ce with
			| Constr (_,lparam,_,b) -> portee_bloc b (ajoute_constr_params_list lparam env)
			| SimpleMethod (_, _, _, lparam, _, e) -> portee_expr e (ajoute_method_params_list lparam env)
			| ComplexMethod (_, _, _, lparam, _, b) -> portee_bloc b (ajoute_method_params_list lparam env)
			| _ -> ()
		) d.ce
	in
	List.iter (fun d -> portee_decl d []) p.classes;
	portee_bloc p.block []
;;

(*
	génère un tableau sous la forme :
	(ClassName, Coef, NomMethod, ListParam)
	avec coef : 0 ni statique ni override
				1 override
				2 statique
				3 les 2
	ListParam :
		(name, type)
 *)
let get_classes_methods_list ld =
	let get_coef c1 c2 =
		(if c1 = true then 2 else 0) + (if c2 = true then 1 else 0)
	in
	let generate_param p =
		List.fold_left (fun acc x -> (x, p.classname_method_param)::acc) [] p.param
	in
	let generate_lparam lparam =
		List.fold_left (fun acc p -> (generate_param p)@acc) [] lparam
	in
	let create_item classname coef name lparam returnedClass =
		(classname, coef, name, lparam, returnedClass)
	in
	let ajoute classname ce env =
		List.fold_left (fun acc e -> match e with
			| Field (_, _, _) -> acc
			| Constr (_, _, _, _) -> acc
			| SimpleMethod (static, override, name, lparam, returnedClass, _) -> (create_item classname (get_coef static override) name (generate_lparam lparam) returnedClass)::acc
			| ComplexMethod (static, override, name, lparam, returnedClass, _) -> (match returnedClass with
				| None -> (create_item classname (get_coef static override) name (generate_lparam lparam) "void")::acc
				| Some s -> (create_item classname (get_coef static override) name (generate_lparam lparam) s)::acc
				)
		) env ce
	in
	List.fold_left (fun acc d -> ajoute d.classname d.ce acc) [] ld
;;

let est_sous_classe c1 c2 env =
	List.fold_left (fun acc (e1, e2) -> acc || (e1 = c1 && e2 = c2)) false env
;;

(*
	La surcharge de méthodes dans une classe ou entre une classe et une super-classe n’est pas autorisée en
	dehors des redéfinitions; elle est autorisée entre méthodes de classes non reliées par héritage. La
	redéfinition doit respecter le profil de la méthode originelle (pas de covariance du type de retour).
 *)

let vc_surcharge ld =
	let methods = get_classes_methods_list ld
	in
	let env = get_class_inheritance_info ld
	in
	(
		List.iter (fun (classname1, so1, methodname1, lparam1, ret1) ->
			List.iter (fun (classname2, so2, methodname2, lparam2, ret2) ->
				if est_sous_classe classname1 classname2 env && methodname1 = methodname2 then (
					(* Printf.printf "super: (%s, %s)::(%s, %s)\n" classname1 classname2 methodname1 methodname2; *)
					if (so1 mod 2) = 1 then (
						if not (lparam1 = lparam2 && ret1 = ret2 && (so1/2) = (so2/2)) then
							raise (VC_Error ("Surcharge de méthode entre une classe et sa super-classe interdite : " ^ classname1 ^ "::" ^ methodname1))
					)
				);
				if classname1 = classname2 && methodname1 = methodname2 then (
					(* Printf.printf "this : (%s, %s)::(%s, %s)\n" classname1 classname2 methodname1 methodname2; *)
					if not (lparam1 = lparam2 && ret1 = ret2 && so1 = so2) then
						raise (VC_Error ("Surcharge interdite dans une classe : " ^ classname1 ^ "::" ^ methodname1)));
				) methods
		) methods
	)
;;

let vc_constr_name d =
	List.iter (fun e ->
		match e with 
			| Constr(name, _, _, _) -> if name = d.classname then () else raise (VC_Error ("Le constructeur et la classe de " ^d.classname ^ " n'ont pas le même nom."))
			| _ -> ()
	) d.ce
;;

let rec vc_integer_string_final d =
	if d.classname = "Integer" || d.classname = "String" then raise (VC_Error ("La classe "^d.classname^" ne peut pas être redéfinie."));
	match d.superClassOpt with
		| Some s -> if s = "Integer" || s = "String" then raise (VC_Error ("La classe "^s^" ne doit pas avoir de sous-classe."))
		| _ -> ()
;;

let vc_one_constr d = 
	let cpt = List.fold_left (fun acc x ->
		match x with
		| Constr (_,_,_,_) -> acc + 1
		| _ -> acc
	) 0 d.ce
	in
	if cpt = 0 then raise (VC_Error ("La classe "^d.classname^" n'a pas de constructeur.")) else 
		if cpt != 1 then raise (VC_Error ("La classe "^d.classname^" a plus d'un constructeur."))
;;

let rec vc_method_result ce =
	let does_expr_have_a_result e =
		match e with
		| Result -> true
		| _ -> false
	in
	let rec does_instr_have_a_result i  =
		match i with
		| Expr e -> (match e with Result -> true | _ -> false)
		| Assignment (e1, _) -> does_expr_have_a_result e1
		| Ite (_, i1, i2) -> does_instr_have_a_result i1 || does_instr_have_a_result i2
		| BlockInstr (bt) -> does_blocktype_have_a_result bt
		| _ -> false
	and
	does_instrlist_have_a_result il =
		List.fold_left (fun acc x -> acc || does_instr_have_a_result x) false il
	and
	does_blocktype_have_a_result bt =
		match bt with
		| Block (il) -> does_instrlist_have_a_result il
		| BlockVar (_, il) -> does_instrlist_have_a_result il
	in
	match ce with
	| ComplexMethod (_, _, _, _, s2, bt) -> (match s2 with
		| None -> if (does_blocktype_have_a_result bt) then raise (VC_Error ("Result in void method"))
		| _ -> if not (does_blocktype_have_a_result bt) then raise (VC_Error ("No result in method"))
	)
	| _ -> ()
;;

let rec is_this_or_super_in_block p =
	let is_expr_this_or_super e =
		match e with
			| This -> true
			| Super -> true
			| _ -> false
	in
	let rec instr_this_or_super l2 = match l2 with
		| Assignment (e1, e2) -> is_expr_this_or_super e1 || is_expr_this_or_super e2
		| Ite (e, i1, i2) ->
			is_expr_this_or_super e;
			instr_this_or_super i1;
			instr_this_or_super i2
		| _ -> false
	in
	let rec is_this_or_super_in_block_sub l = match l with
		| [] -> false
		| x::s -> is_this_or_super_in_block_sub s || instr_this_or_super x
	in
	match p.block with
		| Block(l) -> if is_this_or_super_in_block_sub l then raise (VC_Error ("This or Super in block"))
		| BlockVar(ml, l) -> if is_this_or_super_in_block_sub l then raise (VC_Error ("This or Super in block"))
;;

let vc p =
	vc_redeclaration_class p.classes;
	vc_inheritance_cycle p.classes;
	vc_defined_classes p;
	vc_surcharge p.classes;
	vc_available_methods_fields p;
	vc_portee_id p;
	is_this_or_super_in_block p;
	List.iter (fun d -> 
		vc_constr_name d;
		vc_integer_string_final d;
		vc_one_constr d;
		List.iter (fun e -> vc_method_result e) d.ce
	) p.classes
;;
