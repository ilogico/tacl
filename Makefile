all: tacl

clean:
	rm grammar.ml grammar.mli grammar.cm* lexer.cm* lexer.ml tacl tacl.cm* compiler.cm*
grammar.ml grammar.mli: grammar.mly
	menhir grammar.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

tacl.cmo: tacl.ml
	ocamlc -c tacl.ml

grammar.cmi: grammar.ml
	ocamlc -c grammar.mli
grammar.cmo: grammar.ml grammar.cmi
	ocamlc -c grammar.ml

lexer.cmo: lexer.ml grammar.cmi
	ocamlc -c lexer.ml

tacl: tacl.cmo grammar.cmo lexer.cmo compiler.ml
	ocamlc tacl.cmo grammar.cmo lexer.cmo compiler.ml -o tacl
