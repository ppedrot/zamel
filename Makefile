# $Id: Makefile

OCAMLYACC=ocamlyacc

OCAMLC=ocamlfind ocamlc -w s -syntax "camlp4o" -package threads -thread -package ulex -package xml-light ulexing.cma -package xml-light xml-light.cma -package lablgtk2 lablgtk.cma gtkInit.cmo unix.cma threads.cma gtkThread.cmo

OCAMLOPT=ocamlfind ocamlopt -w s -syntax "camlp4o" -package threads -thread -package ulex ulexing.cmxa -package xml-light xml-light.cmxa -package lablgtk2 lablgtk.cmxa gtkInit.cmx unix.cmxa threads.cmxa gtkThread.cmx

OCAMLDEP=ocamlfind ocamldep -syntax "camlp4o" -package ulex

INCLUDES=-I parsing -I gui

OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)
OCAMLYACCFLAGS=
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=ipadata.cmo converter.cmo binary.cmo regexp.cmo data_set.cmo parsing/rule_parser.cmo parsing/rule_lexer.cmo parsing/word_parser.cmo parsing/word_lexer.cmo parsing/converter_parser.cmo parsing/converter_lexer.cmo gui/dialog.cmo gui/gui_tools.cmo gui/save.cmo

OPTFILES=ipadata.cmx converter.cmx binary.cmx regexp.cmx data_set.cmx parsing/rule_parser.cmx parsing/rule_lexer.cmx parsing/word_parser.cmx parsing/word_lexer.cmx parsing/converter_parser.cmx parsing/converter_lexer.cmx gui/dialog.cmx gui/gui_tools.cmx gui/save.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


all: yacc $(BYTEFILES)
	$(OCAMLC) $(INCLUDES) $(BYTEFILES) -o zamel gui/main.ml

opt: yacc $(OPTFILES)
	$(OCAMLOPT) $(INCLUDES) $(OPTFILES) -o zamel gui/main.ml

dep: yacc
	$(OCAMLDEP) $(OCAMLDEPFLAGS) parsing/*.ml parsing/*.mli gui/*.ml gui/*.mli *.ml *.mli > .depend

yacc:
	for file in `ls parsing/*.mly`; do $(OCAMLYACC) $(OCAMLYACCFLAGS) $$file; done

clean:
	rm -f *.cm[oix] *.o
	rm -f parsing/*.cm[oix] parsing/*.o parsing/*_parser.ml parsing/*_parser.mli
	rm -f gui/*.cm[oix] gui/*.o

include .depend