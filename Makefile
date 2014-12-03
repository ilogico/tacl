all: tacl

clean:
	rm grammar.ml grammar.mli *.cmi *.o *.cmx lexer.ml tacl
grammar.ml grammar.mli: grammar.mly
	menhir grammar.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

tacl.cmx tacl.o: tacl.ml
	ocamlopt -c tacl.ml

grammar.cmi: grammar.ml
	ocamlopt -c grammar.mli
grammar.cmx: grammar.ml grammar.cmi
	ocamlopt -c grammar.ml

lexer.cmx lexer.o: lexer.ml grammar.cmi
	ocamlopt -c lexer.ml

compiler.cmx compiler.o: compiler.ml
	ocamlopt -c compiler.ml

tacl: tacl.cmx tacl.o grammar.cmx grammar.o lexer.cmx lexer.o compiler.cmx compiler.o
	ocamlopt -o tacl tacl.cmx grammar.cmx lexer.cmx compiler.cmx
