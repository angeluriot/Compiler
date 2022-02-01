open Ast

(*let constr_name_equals_class_name p =
	let rec check_class c l =
		let rec check_element c e =
			match e with
			| Constr(name, _, _, _) -> if name = c then return true else false
			| _ -> false
		in
		match l with
		| _::s -> check_element c s
		| [] -> false
	in
	let*)

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
