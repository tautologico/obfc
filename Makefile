all: obfc

obfc: strPath.ml file.ml absSyntax.ml optim.ml semAnl.ml cTempl.ml cGen.ml compiler.ml
	ocamlc -o obfc strPath.ml file.ml absSyntax.ml optim.ml semAnl.ml cTempl.ml cGen.ml compiler.ml

clean:
	rm *~
	rm *.cm*
	rm obfc

