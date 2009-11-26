once:
	ocamlopt ./simulacrum.ml -o ./simulacrum
	./simulacrum
hundred:
	rm -f /tmp/results
	ocamlopt ./simulacrum.ml -o simulacrum
	./runit.sh 100
thousand:
	rm -f /tmp/results
	ocamlopt ./simulacrum.ml -o simulacrum
	./runit.sh 1000
