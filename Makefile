UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
EXT :=
else
EXT := .exe
endif

native: lexer
	@echo Building native executable:
	@ocamlbuild -r -build-dir bin -j 4 -I src -I bin/generated -use-menhir main.native
	@cp bin/src/main.native scc$(EXT)

lexer:
	@echo Generating Lexer...
	@mkdir -p bin/generated
	@ocamllex -ml -o bin/generated/lexer.ml src/lexer.mll

test:
	@echo Running application...
	@./scc$(EXT)

clean:
	@echo Cleaning workspace...
	@ocamlbuild -build-dir bin -clean