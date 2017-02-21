all: test

test:
	ocamlbuild -package oUnit tests.native
	mv tests.native bin/ounit

clean:
	ocamlbuild -clean