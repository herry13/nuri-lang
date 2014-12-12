.PHONY: build clean clean-all dist

# 0 = OCaml bytecode
# 1 = native
NATIVE=0

# binary name
NAME=nuric

# 0 = stack trace disabled, 1 = stack trace enabled
# Don't forget to set environment variable OCAMLRUNPARAM=b
STACK_TRACE=1

PACKAGES=#-package yojson
OCAMLFIND=#ocamlfind -linkpkg $(PACKAGES)

ifeq ($(NATIVE),1)
# native compiler
#COMPILER=ocamlfind ocamlopt -S -linkpkg $(PACKAGES)
COMPILER=$(OCAMLFIND) ocamlopt -S
else
# byte code compiler
ifeq ($(STACK_TRACE),1)
#COMPILER=ocamlfind ocamlc -linkpkg -g $(PACKAGES)
COMPILER=$(OCAMLFIND) ocamlc -g
else
#COMPILER=ocamlfind ocamlc -linkpkg $(PACKAGES)
COMPILER=$(OCAMLFIND) ocamlc
endif
endif

# byte code libraries
BYTECODELIB= \
	str.cma \
    unix.cma \
	common.cmo \
	syntax.cmo \
	lexer.cmo \
	parser.cmo \
	parser_helper.cmo \
	domain.cmo \
	type.cmo \
	json.cmo \
	variable.cmo \
	constraint.cmo \
	action.cmo \
	valuation.cmo \
	plan.cmo \
	fdr.cmo \
	nuric.cmo

# native code libraries
NATIVELIB= \
	str.cmxa \
    unix.cmxa \
	common.cmx \
	syntax.cmx \
	lexer.cmx \
	parser.cmx \
	parser_helper.cmx \
	domain.cmx \
	type.cmx \
	json.cmx \
	variable.cmx \
	constraint.cmx \
	action.cmx \
	valuation.cmx \
	plan.cmx \
	fdr.cmx \
	nuric.cmx

# default
build: nuric

nuric: nuric.ml action parser_helper valuation type fdr plan
	$(COMPILER) -c nuric.ml
ifeq ($(NATIVE),1)
	$(COMPILER) -o $(NAME) $(NATIVELIB)
else
	$(COMPILER) -o $(NAME) $(BYTECODELIB)
endif

syntax: syntax.mli syntax.ml
	$(COMPILER) -c syntax.mli
	$(COMPILER) -c syntax.ml

parser: parser.mly syntax
	ocamlyacc parser.mly
	$(COMPILER) -c parser.mli
	$(COMPILER) -c parser.ml

lexer: lexer.mll parser
	ocamllex lexer.mll
	$(COMPILER) -c lexer.ml

parser_helper: parser_helper.ml parser lexer
	$(COMPILER) -c parser_helper.ml

json: domain type json.mli json.ml
	$(COMPILER) -c json.mli
	$(COMPILER) -c json.ml

domain: common syntax domain.mli domain.ml
	$(COMPILER) -c domain.mli
	$(COMPILER) -c domain.ml

valuation: valuation.mli valuation.ml domain
	$(COMPILER) -c valuation.mli
	$(COMPILER) -c valuation.ml

type: domain type.mli type.ml
	$(COMPILER) -c type.mli
	$(COMPILER) -c type.ml

common: common.mli common.ml
	$(COMPILER) -c common.mli
	$(COMPILER) -c common.ml

variable: variable.mli variable.ml common domain type json
	$(COMPILER) -c variable.mli
	$(COMPILER) -c variable.ml

constraint: variable common domain type constraint.mli constraint.ml
	$(COMPILER) -c constraint.mli
	$(COMPILER) -c constraint.ml

action: common domain type variable constraint action.mli action.ml
	$(COMPILER) -c action.mli
	$(COMPILER) -c action.ml

fdr: common domain type json variable constraint action plan fdr.mli fdr.ml
	$(COMPILER) -c fdr.mli
	$(COMPILER) -c fdr.ml

plan: domain plan.mli plan.ml
	$(COMPILER) -c plan.mli
	$(COMPILER) -c plan.ml

clean:
	@rm -f *.s *.cmo parser.mli lexer.mli *.cmi *.o *.cmx parser.ml lexer.ml parser.output ocamlprof.dump

clean-all: clean
	@rm -f $(NAME)

dist: build clean

clean-build: clean-all build

travis: dist

test:
	@bash ../test/test-ocaml.sh

DOC_OPTIONS=-html

DOC_SOURCE_FILES= \
    syntax.mli \
    domain.mli \

doc:
	ocamldoc $(DOC_OPTIONS) $(DOC_SOURCE_FILES)
	@mkdir -p ../doc
	@mv -f *.{html,css} ../doc
