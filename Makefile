# Compilateurs
OCC = ocamlopt
OCL = ocamllex
OCY = ocamlyacc

# Fichiers compilés, à produire pour fabriquer l'exécutable
OBJS = lex.cmx ast.cmx parse.cmx sem.cmx loop.cmx

loop: $(OBJS)
	$(OCC) -o $@ $(OBJS)

# Les cibles auxiliaires
# (note: une cible avec  « :: » peut être étendue par la suite)
clean::
	/bin/rm -f *~ *.cmo *.cmx *.o *.cmi *.cmt *.cmti \
                   parse.ml parse.mli lex.ml loop


# Les dépendances
loop.cmx: ast.cmi parse.cmi lex.cmi

lex.cmx: parse.cmi

parse.cmi: ast.cmi

parse.cmx: ast.cmi

sem.cmx: ast.cmi

parse.mli: parse.ml

ast.cmi: ast.cmx

lex.cmi: lex.cmx

# Générations de fichiers compilés selon leurs extensions (suffixes) :
# .ext1.ext2 : comment passer de foo.ext1 à foo.ext2
.ml.cmx:
	$(OCC) -c $<

.mli.cmi:
	$(OCC) -c $<

.mll.ml:
	$(OCL) $<

.mly.ml:
	$(OCY) $<

# Déclaration de suffixes :
#  - d'abord, on supprime les suffixes connus de make (.c, .o, etc.)
.SUFFIXES:

# - ensuite, on déclare nos suffixes
.SUFFIXES: .ml .mli .mly .mll .cmx .cmi

