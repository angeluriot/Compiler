open Ast

(* mettre ici des fonctions très générales qui peuvent être utiles à plusieurs
 * endroits et qui ne dépendent pas d'autres modules
 *)

let string_of_relop (op: Ast.opComp)  =
  match op with
    Eq -> "="
  | Neq -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

