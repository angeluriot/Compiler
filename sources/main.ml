open Ast
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  (* lance l'analyseur syntaxique, qui se charge d'appeler l'analyseur lexical
   * a chaque fois qu'il a besoin d'un nouveau token.
   * Parse.prog est le nom de la fonction produite par Menhir pour l'axiome
   * nommé prog de la grammaire contenue dans parse.mly
   * La valeur retournée est la valeur retournée par l'action associée à la
   * production de l'axiome.
   *
   * Normalement ici on devrait récupérer une paire consitituée de l'AST pour
   * la partie declarations et l'AST pour l'expression contenue entre le begin
   * et le end dans le programme qu'on analyse.
   * La suite peut par exemple consister à parcourir les AST construits de
   * façon à en imprimer le contenu sous forme non ambigue, par exemple pour
   * vérifier la précédence et l'associativité des opérateurs.
   * Ensuite faire les éventuelles vérifications contextuelles, enfin de lancer
   * la partie évaluation (cas d'un interprète) ou génération de code (cas d'un
   * compilateur)
   *)
  try let (ld, e) = Parse.prog Lex.token lexbuf in
  Print.print ld e;
  with
    Parse.Error -> (* exception levee en cas d'erreur syntaxique *)
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let _ =
  (* on  doit passer en argument sur la ligne de commande le nom du fichier qui
   * contient le programme à compiler.
   * Le lexer est produit par ocamllex à partir du fichier lex.mll
   *)

  let file = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel file in
  let _ = parse_with_error lexbuf
  in close_in file
