Type opComp = 
  Eq | Neq | Lt | Le | Gt | Ge

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

}
type classe = {
    nom: string
    param: expr list    
    superclasseoption: option
    constructeur: int
    corps: instructions
}

type bloc = instructions list * decl list 

type instruction = Expr | Bloc | Return | Affectation of expr*expr | Ite

type instructions = Instruction list

type 'a option = Some of 'a | None


