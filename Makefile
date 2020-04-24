default : hello
OCAMLC=ocamlfind ocamlc ${LIB}
OCAMLOPT=ocamlfind ocamlc ${LIB}
OCAMLDEP=ocamlfind ocamldep
OCAMLYACC=menhir

LIB=-package sexplib,ppx_sexp_conv
LLIB=

OBJS=location.cmo parse_tree.cmo parser.cmo lexer.cmo main.cmo
OPT=-g

f90: ${OBJS}
	$(OCAMLC) ${LLIB} ${OPT} -w -A -linkpkg -linkall -o $@ ${OBJS}

all: clean f90 test

depend:
	$(OCAMLDEP) *.ml > .depend

hello: location.ml parse_tree.ml lexer.ml
	ocamlopt -o $@ $^
	cat location.ml
	cat parse_tree.ml
	cat lexer.ml
test:
	for file in `find test -name *.f90 | sort`; do \
	echo $${file} ":"; \
	./f90 $${file}; \
	done \

clean:
	rm -rf *.cm? parser.ml parser.mli lexer.ml *.output

%.re : %.ml
	refmt -p re $< > $@
%.rei : %.mli
	refmt -p rei $< > $@
all: location.ml parse_tree.ml
	ocamlopt -o $@ $^
	clean f90 test
parser.cmo: parser.ml parser.cmi
	$(OCAMLC) -c $<

parser.mli: parser.mly
	$(OCAMLYACC) -v $<

%.cmi: %.mli
	${OCAMLC} -c ${LIB} ${OPT} $<
%.cmo: %.ml
	${OCAMLC} -c ${LIB} ${OPT} $<
%.cmx: %.ml
	${OCAMLOPT} -c ${LIB} ${OPT} $<
%.ml: %.mll
	ocamllex $<
%.ml: %.mly
	menhir -v $<

.PHONY: clean test

include .depend
