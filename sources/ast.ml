Type opComp = 
  Eq | Neq | Lt | Le | Gt | Ge
(*type ident = { name : string; typ : typ }*)
type expr =
  Id of string
| Selection of expr*expr
| Instanciation of classe*expr list (*new est un mot clef / arguments = list de expr*)
| EnvoiMessage of expr*expr list
| Plus of expr*expr
| Cste of int
| Minus of expr*expr
| Div of expr*expr
| UMinus of expr
| Comp of opComp*expr*expr

  
type selection = {
    lexpr: expr
    rnom: string
}

type cast = {
    nomClass: string
    expression: expr
}

type decl = {
    classname: string;
    lparam: constructorParameter list;
    superclasseOpt: option;
    ce1: classElement list;
    c: constructor;
    ce2: classElement list;
}

type constructor = {
    classname: string;
    lparam: constructorParameter list;
    superclasseOpt: option;
    corps: instructions;
}

type constructorParameter = {
    superclasseOpt: option;
    param: expr list;
    classname: string;
}

type parametre = {
	nom: string;
}

type classe = {
    nom: string;
    param: expr list;
    superclasseoption: option;
    constructeur: int;
    corps: instructions;
}

type instruction = Expr | Bloc | Return | Affectation of expr*expr | Ite

type instructions = instruction list

type 'a option = Some of 'a | None

type methodParameter = {
	param: expr list;
	classname: string;
}

type bloc = {
	l: instructions;
	var: methodParameter list;
	li:  instructions;
}

type selection = {
	e: expr;
	x: string; (*Possibilité de mettre x:string; et de laisser dans type expr id : string*)
}

type message = {
	x: string;
	lparam: expr list;
}

type instruction = {
	e: expr;
	b: bloc;
	x: string ASSIGN e = expr;
	s: selection ASSIGN e = expr;
	IF si = expr THEN alors = instruction ELSE sinon = instruction { Ite(si, alors, sinon) };
	
}
