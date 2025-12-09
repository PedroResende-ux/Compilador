# Makefile para o compilador Ada

# Compilador Haskell
GHC = ghc
GHCFLAGS = -dynamic

# Ferramentas de geração
ALEX = alex
HAPPY = happy

# Ficheiros fonte
LEXER_SRC = Lexer.x
PARSER_SRC = Parser.y
MAIN_SRC = Main.hs
AST_SRC = AST.hs
SEMANTIC_SRC = Semantic.hs

# Ficheiros gerados
LEXER_HS = Lexer.hs
PARSER_HS = Parser.hs

# Executável
EXEC = compilador

all: $(EXEC)

# Gerar o lexer a partir do ficheiro .x
$(LEXER_HS): $(LEXER_SRC)
	$(ALEX) $(LEXER_SRC)

# Gerar o parser a partir do ficheiro .y
$(PARSER_HS): $(PARSER_SRC)
	$(HAPPY) $(PARSER_SRC)

# Compilar o programa principal
$(EXEC): $(LEXER_HS) $(PARSER_HS) $(AST_SRC) $(SEMANTIC_SRC) $(MAIN_SRC)
	$(GHC) $(GHCFLAGS) --make $(MAIN_SRC) -o $(EXEC)

# Testar com um exemplo
test: $(EXEC)
	./$(EXEC) test.ada

# Limpar ficheiros gerados
clean:
	rm -f $(LEXER_HS) $(PARSER_HS)
	rm -f *.hi *.o *.dyn_hi *.dyn_o
	rm -f $(EXEC)
	rm -f Parser.info

.PHONY: all test clean