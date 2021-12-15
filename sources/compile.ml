open Ast

let rec compileExpr e env chan  = () (* a faire *)

;;


(* par defaut on stocke le code engendré dans un fichier qui s'appelle out.txt
 * Ci-dessous on crée un descripteur de fichier (canal de communication) qu'on
 * fournit en argument à la fonction output_string avec le texte voulu.
 * Les instructions à écrire sont celles du code de la machine virtuelle vue
 * en cours
 *)
let compile ld e =
  let chan = open_out "out.txt" in
  output_string chan "START\n";
  (* ici traiter les declarations (dans le bon ordre) ainsi que l'expression
   * principale du programme
   *)
  output_string chan "STOP\n";
  (* gere le contenu qui reste eventuellement dans le buffer de sortie et
   * ferme le descripteur de fichier
   *)
  flush chan;
  close_out chan;
;;
