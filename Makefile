# $Id: Makefile

OCAMLYACC=ocamlyacc

OCAMLC=ocamlfind ocamlc -w s -syntax "camlp4o" -package ulex -package xml-light ulexing.cma -package xml-light xml-light.cma

OCAMLOPT=ocamlfind ocamlopt -w s -syntax "camlp4o" -package threads -thread -package ulex ulexing.cmxa -package xml-light xml-light.cmxa -package lablgtk2 lablgtk.cmxa gtkInit.cmx unix.cmxa threads.cmxa gtkThread.cmx

OCAMLDEP=ocamlfind ocamldep -syntax "camlp4o" -package ulex

OCAMLJS=js_of_ocaml

INCLUDES=-I parsing -I gui -I web

OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)
OCAMLYACCFLAGS=
OCAMLDEPFLAGS=$(INCLUDES)

BYTEFILES=ipadata.cmo converter.cmo binary.cmo regexp.cmo data_set.cmo \
parsing/rule_parser.cmo parsing/rule_lexer.cmo parsing/word_parser.cmo \
parsing/word_lexer.cmo parsing/converter_parser.cmo \
parsing/converter_lexer.cmo

GUIFILES=gui/dialog.cmo gui/gui_tools.cmo gui/save.cmo

WEBFILES=web/web.cmo

OPTFILES=$(BYTEFILES:.cmo=.cmx) $(GUIFILES:.cmo=.cmx)

GUIFLAGS=-package threads -thread -package lablgtk2 lablgtk.cma gtkInit.cmo \
unix.cma threads.cma gtkThread.cmo

# Common rules
web/%.cmo: web/%.ml
	$(OCAMLC) $(OCAMLFLAGS) -package js_of_ocaml.syntax -package js_of_ocaml -c $<

gui/%.cmo: gui/%.ml
	$(OCAMLC) $(OCAMLFLAGS) $(GUIFLAGS) -c $<

gui/%.cmi: gui/%.mli
	$(OCAMLC) $(OCAMLFLAGS) $(GUIFLAGS) -c $<

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<


all: yacc $(BYTEFILES) $(GUIFILES)
	$(OCAMLC) $(OCAMLFLAGS) $(GUIFLAGS) $(BYTEFILES) $(GUIFILES) -o zamel gui/main.ml

opt: yacc $(OPTFILES)
	$(OCAMLOPT) $(INCLUDES) $(OPTFILES) -o zamel gui/main.ml

dep: yacc
	$(OCAMLDEP) $(OCAMLDEPFLAGS) parsing/*.ml parsing/*.mli gui/*.ml gui/*.mli *.ml *.mli > .depend
	$(OCAMLDEP) $(OCAMLDEPFLAGS) -package js_of_ocaml.syntax web/*.ml web/*.mli >> .depend

yacc:
	for file in `ls parsing/*.mly`; do $(OCAMLYACC) $(OCAMLYACCFLAGS) $$file; done

clean:
	rm -f *.cm[oix] *.o
	rm -f parsing/*.cm[oix] parsing/*.o parsing/*_parser.ml parsing/*_parser.mli
	rm -f gui/*.cm[oix] gui/*.o
	rm -f web/*.cm[oi] web/*.js

web/zamel.js: $(BYTEFILES) $(WEBFILES)
	$(OCAMLC) $(INCLUDES) $(BYTEFILES) $(WEBFILES) -package js_of_ocaml -linkpkg -o zamel
	$(OCAMLJS) -I data -I test --extern-fs --file="latin.lex" --file="ancient_french.sc" --file="ipadata.xml" -o web/zamel.js zamel

web: web/zamel.js

-include .depend
