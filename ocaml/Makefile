.PHONY: build clean clean-all dist

# 0 = OCaml bytecode
# 1 = native
NATIVE = 0

# binary name
BIN = nuric

# 0 = stack trace disabled, 1 = stack trace enabled
# Don't forget to set environment variable OCAMLRUNPARAM=b
STACK_TRACE = 0

# !!! NO TRAILING WHITESPACE
PACKAGES = str unix oUnit

# List modules (order is important!)
MODULES = \
    common \
    syntax \
    lexer \
    parser \
    parser_helper \
    domain \
    type \
    typechecker \
    nuri_json \
    variable \
    constraint \
    action \
    valuation \
    plan \
    fdr \
    nuric \

ifeq ($(PACKAGES), )
OCAMLFIND =
OCAMLOPTS =
else
OCAMLFIND = ocamlfind
OCAMLOPTS = -linkpkg \
            -package $(shell echo "$(PACKAGES)"|sed 's/ / -package /g')
endif

ifeq ($(NATIVE), 1) # native compiler
COMPILER=$(OCAMLFIND) ocamlopt -S $(OCAMLOPTS)
LIB = $(shell echo "$(MODULES)"|sed 's/ /.cmx /g')
else
LIB = $(shell echo "$(MODULES)"|sed 's/ /.cmo /g')
ifeq ($(STACK_TRACE), 1) # bytecode compiler with stack trace
COMPILER=$(OCAMLFIND) ocamlc -g $(OCAMLOPTS)
else # bytecode compiler without stack trace
COMPILER=$(OCAMLFIND) ocamlc $(OCAMLOPTS)
endif
endif

# default
build: nuric

nuric: nuric.ml action parser_helper typechecker valuation fdr plan
	$(COMPILER) -c nuric.ml
	$(COMPILER) -o $(BIN) $(LIB)

js: nuric.ml action parser_helper typechecker valuation fdr plan
	$(COMPILER) -c nuric.ml
	$(COMPILER) -o $(BIN) $(LIB) -package js_of_ocaml -syntax camlp4o -package js_of_ocaml.syntax
	js_of_ocaml $(BIN) --disable genprim

syntax: common syntax.mli syntax.ml
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

nuri_json: domain type nuri_json.mli nuri_json.ml
	$(COMPILER) -c nuri_json.mli
	$(COMPILER) -c nuri_json.ml

domain: common syntax domain.mli domain.ml
	$(COMPILER) -c domain.mli
	$(COMPILER) -c domain.ml

domain_test: domain test/domain_test.ml
	$(COMPILER) -g -c test/domain_test.ml
	$(COMPILER) -g -o domain_test common.cmo domain.cmo test/domain_test.cmo
	./domain_test -verbose true -no-cache-filename -no-output-file -output-html-dir test

valuation: valuation.mli valuation.ml domain
	$(COMPILER) -c valuation.mli
	$(COMPILER) -c valuation.ml

type: domain type.mli type.ml
	$(COMPILER) -c type.mli
	$(COMPILER) -c type.ml

typechecker: domain type
	$(COMPILER) -c typechecker.mli
	$(COMPILER) -c typechecker.ml

common: common.mli common.ml
	$(COMPILER) -c common.mli
	$(COMPILER) -c common.ml

variable: variable.mli variable.ml common domain type nuri_json
	$(COMPILER) -c variable.mli
	$(COMPILER) -c variable.ml

constraint: variable common domain type constraint.mli constraint.ml
	$(COMPILER) -c constraint.mli
	$(COMPILER) -c constraint.ml

action: common domain type variable constraint action.mli action.ml
	$(COMPILER) -c action.mli
	$(COMPILER) -c action.ml

fdr: common domain type nuri_json variable constraint action plan fdr.mli fdr.ml
	$(COMPILER) -c fdr.mli
	$(COMPILER) -c fdr.ml

plan: domain plan.mli plan.ml
	$(COMPILER) -c plan.mli
	$(COMPILER) -c plan.ml

clean:
	@rm -f *.s *.cm{i,o,x} *.o
	@rm -f test/*.cm{i,o} test/index.html test/oUnit.{css,js}
	@rm -f parser.mli lexer.mli parser.ml lexer.ml parser.output ocamlprof.dump
	@rm -f oUnit-anon.cache domain_test

clean-all: clean
	@rm -f $(BIN)

dist: build clean

clean-build: clean-all build

travis: dist

test: domain_test

DOC_OPTIONS=-html

DOC_SOURCE_FILES= \
	syntax.mli \
	domain.mli \

doc:
	ocamldoc $(DOC_OPTIONS) $(DOC_SOURCE_FILES)
	@mkdir -p ../doc
	@mv -f *.{html,css} ../doc
