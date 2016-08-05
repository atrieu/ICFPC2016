all: main.native

clean:
	ocamlbuild -clean

main.native: main.ml spec.ml parse_problem.ml print_solution.ml
	ocamlbuild -lib nums main.native
