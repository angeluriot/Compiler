CP_FOLDER  = sources
GEN_FOLDER = build

CP_FILES   = ast.ml compile.ml eval.ml main.ml utils.ml print.ml testLex.ml lex.mll parse.mly
COPIES     = $(addprefix $(CP_FOLDER)/, $(CP_FILES))

INTERFACES = parse.mli
SOURCES    = ast.ml utils.ml print.ml eval.ml compile.ml parse.ml lex.ml main.ml
GENERATED  = lex.ml parse.ml parse.mli parse.automaton parse.conflicts

all: copy $(GEN_FOLDER)/parse.mli $(addprefix $(GEN_FOLDER)/, $(SOURCES))
	cd $(GEN_FOLDER) ; ocamlc -c ast.ml
	cd $(GEN_FOLDER) ; ocamlc $(INTERFACES)
	cd $(GEN_FOLDER) ; ocamlc -o tp $(SOURCES)

testLex: copy $(GEN_FOLDER)/parse.mli $(GEN_FOLDER)/lex.ml $(GEN_FOLDER)/testLex.ml $(GEN_FOLDER)/utils.ml
	cd $(GEN_FOLDER) ; ocamlc -c ast.ml
	cd $(GEN_FOLDER) ; ocamlc $(INTERFACES)
	cd $(GEN_FOLDER) ; ocamlc -o testLex ast.ml utils.ml print.ml eval.ml compile.ml parse.ml  lex.ml testLex.ml

$(GEN_FOLDER)/lex.ml: copy $(GEN_FOLDER)/lex.mll $(GEN_FOLDER)/parse.mli $(GEN_FOLDER)/ast.ml
	cd $(GEN_FOLDER) ; ocamllex lex.mll

$(GEN_FOLDER)/parse.mli: $(GEN_FOLDER)/parse.mly $(GEN_FOLDER)/ast.ml
	cd $(GEN_FOLDER) ; menhir --dump --explain --strict parse.mly

copy:
	cp $(COPIES) $(GEN_FOLDER)

clean:
	rm -rf $(GEN_FOLDER)/*