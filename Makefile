

native: lexer
	@echo Building native executable:
	@ocamlbuild -r -build-dir bin -j 4 -I src -I bin/generated -use-menhir main.native
	@cp bin/src/main.native scc

lexer:
	@echo Generating Lexer...
	@mkdir -p bin/generated
	@ocamllex -ml -o bin/generated/lexer.ml src/lexer.mll


clean:
	@echo Cleaning workspace...
	@ocamlbuild -build-dir bin -clean