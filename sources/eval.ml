open Ast

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
