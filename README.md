# Compilador Ada - Trabalho Prático 1
**Compiladores - DCC FCUP**  
**Novembro 2025**

## Identificação do Grupo
- **Aluno 1:** [Nome Completo] - [Número de estudante]
- **Aluno 2:** [Nome Completo] - [Número de estudante] 

---

## Descrição do Projeto

Este projeto implementa um **compilador para um subconjunto da linguagem Ada**, conforme especificado no enunciado do Trabalho Prático 1. O compilador realiza as fases de:

1. **Análise Léxica** (Lexer) - Reconhecimento de tokens
2. **Análise Sintática** (Parser) - Construção da árvore sintática abstrata (AST)
3. **Análise Semântica** (Semantic) - Validação de declarações e escopos com tabela de símbolos

O compilador aceita como entrada um programa Ada válido e produz como saída:
- Árvore sintática abstrata (AST) formatada
- Resultados da análise semântica (erros e warnings)
- Tabela de símbolos com todas as declarações

---

## Funcionalidades Implementadas

### 📝 Análise Léxica (Lexer)

Implementada usando **Alex**, o lexer reconhece:

#### Palavras-chave (case-insensitive):
- Estrutura de programa: `procedure`, `is`, `begin`, `end`
- Controlo de fluxo: `if`, `then`, `else`, `while`, `loop`
- Operadores lógicos: `and`, `or`, `not`
- Valores booleanos: `True`, `False`
- Operador aritmético: `mod`
- Input/Output: `Put_Line`, `Get_Line`
- **Tipos de dados:** `Integer`, `Boolean`

#### Operadores:
- **Aritméticos:** `+`, `-`, `*`, `/`, `mod`
- **Relacionais:** `=`, `/=`, `<`, `<=`, `>`, `>=`
- **Lógicos:** `and`, `or`, `not`
- **Atribuição:** `:=`

#### Literais:
- **Inteiros:** `42`, `0`, `123`
- **Strings:** `"Hello World"`, `"Ada"`
- **Booleanos:** `True`, `False`

#### Outros:
- **Identificadores:** variáveis (e.g., `x`, `counter`, `my_var`)
- **Comentários:** linhas começadas por `--` (ignorados)
- **Pontuação:** `;`, `(`, `)`, `:`

**Nota importante:** As palavras-chave são **case-insensitive**, como na linguagem Ada real. Assim, `BEGIN`, `Begin` e `begin` são todos reconhecidos.

---

### 🌳 Análise Sintática (Parser)

Implementada usando **Happy**, o parser reconhece:

#### Estrutura do Programa:
```ada
procedure Main is
  -- declarações de variáveis (opcional)
  x : Integer;
  y : Boolean;
begin
  -- comandos aqui
end Main;
```

O programa **deve** ser uma procedure chamada `Main` (case-insensitive), e o nome no início e no fim devem coincidir.

#### Declarações de Variáveis:
```ada
x : Integer;
counter : Integer;
flag : Boolean;
```
- Devem aparecer entre `is` e `begin`
- Formato: `identificador : Tipo ;`
- Tipos suportados: `Integer`, `Boolean`

#### Comandos Suportados:

1. **Atribuição:**
   ```ada
   x := 10;
   y := x + 5;
   ```

2. **Condicional (if-then-else):**
   ```ada
   if x > 5 then
     Put_Line("maior")
   else
     Put_Line("menor");
   ```

3. **Condicional sem else:**
   ```ada
   if x > 0 then
     Put_Line("positivo");
   ```

4. **Ciclo while:**
   ```ada
   while x < 10 loop
     x := x + 1
   end loop;
   ```

5. **Blocos:**
   ```ada
   begin
     x := 1;
     y := 2
   end;
   ```

6. **Output:**
   ```ada
   Put_Line("Hello World");
   Put_Line(x + y);
   ```

#### Expressões Suportadas:

- **Aritméticas:** `x + y`, `a * b - c`, `(x + y) / 2`
- **Relacionais:** `x > 5`, `a <= b`, `x = y`, `a /= b`
- **Lógicas:** `flag and (x > 0)`, `a or b`, `not flag`
- **Negação:** `-x`, `-(a + b)`
- **Módulo:** `x mod 3`
- **Input:** `Get_Line` (lê entrada do utilizador)
- **Parênteses:** `(x + y) * z`

#### Precedência de Operadores (da menor para a maior):
1. `:=` (atribuição)
2. `or`
3. `and`
4. `=`, `/=`, `<`, `<=`, `>`, `>=` (não-associativos)
5. `+`, `-`
6. `*`, `/`, `mod`
7. `not`, negação unária (`-`)

---

### 🗂️ Árvore Sintática Abstrata (AST)

A AST é definida no módulo `AST.hs`:

```haskell
-- Programa
data Program = Program [Decl] [Stmt]

-- Declarações
data Decl = VarDecl String Type

-- Tipos
data Type = IntegerType | BooleanType

-- Comandos
data Stmt = 
    Assignment String Expr
  | IfThenElse Expr Stmt Stmt
  | IfThen Expr Stmt
  | While Expr Stmt
  | Block [Stmt]
  | PutLine Expr
  | EmptyStmt

-- Expressões
data Expr = 
    IntLit Int | BoolLit Bool | StringLit String | Var String
  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
  | Mod Expr Expr | Neg Expr
  | And Expr Expr | Or Expr Expr | Not Expr
  | Eq Expr Expr | Neq Expr Expr
  | Lt Expr Expr | Lte Expr Expr | Gt Expr Expr | Gte Expr Expr
  | GetLine
```

---

### 🔍 Análise Semântica (Semantic Analysis)

Implementada no módulo `Semantic.hs`, realiza:

#### Tabela de Símbolos:
- Armazena todas as declarações de variáveis
- Mantém informação de tipo (`Integer` ou `Boolean`)
- Gerencia escopos aninhados (blocos `begin...end`)
- Cada escopo tem seu próprio namespace

#### Validações Realizadas:

1. **Detecção de Redeclarações:**
   ```ada
   x : Integer;
   x : Integer;  -- ERRO!
   ```
   Erro: "Variable 'x' already declared in this scope"

2. **Detecção de Variáveis Não Declaradas:**
   ```ada
   procedure Main is
     x : Integer;
   begin
     y := 10  -- ERRO: y não foi declarada
   end Main;
   ```
   Erro: "Variable 'y' used but not declared"

3. **Gestão de Escopos:**
   ```ada
   procedure Main is
     x : Integer;
   begin
     x := 10;       -- OK
     begin
       x := 20;     -- OK: acessa x do escopo exterior
     end
   end Main;
   ```

#### Saída da Análise Semântica:
- Lista de erros semânticos (se houver)
- Lista de warnings (se houver)
- Tabela de símbolos final
- Programa termina com erro se houver erros semânticos

---

## Estrutura do Projeto

```
.
├── AST.hs                      # Definição da AST e Tabela de Símbolos
├── Semantic.hs                 # Análise semântica
├── Lexer.x                     # Especificação do lexer (Alex)
├── Parser.y                    # Especificação do parser (Happy)
├── Main.hs                     # Programa principal
├── Makefile                    # Automatização da compilação
├── test.ada                    # Programa de teste principal
├── test_declarations.ada       # Teste de declarações
├── test_undeclared.ada         # Teste de erro: variável não declarada
├── test_redeclaration.ada      # Teste de erro: redeclaração
├── test_nested_scope.ada       # Teste de escopos aninhados
├── test_comprehensive.ada      # Teste abrangente
├── SYMBOL_TABLE_IMPLEMENTATION.md  # Documentação da tabela de símbolos
└── README.md                   # Este ficheiro
```

---

## Requisitos e Instalação

### Pré-requisitos

- **GHC** (Glasgow Haskell Compiler) - versão 8.10 ou superior
- **Alex** - gerador de analisadores léxicos
- **Happy** - gerador de analisadores sintáticos
- **Make** (opcional, para usar o Makefile)

### Instalação no Ubuntu/Debian

```bash
# Instalar GHC e ferramentas base
sudo apt-get update
sudo apt-get install ghc cabal-install

# Atualizar cabal
cabal update

# Instalar Alex e Happy
cabal install alex happy

# Adicionar ao PATH (se necessário)
echo 'export PATH="$HOME/.cabal/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc
```

### Instalação no macOS (com Homebrew)

```bash
brew install ghc cabal-install
cabal update
cabal install alex happy
```

### Instalação usando Stack (alternativa)

```bash
curl -sSL https://get.haskellstack.org/ | sh
stack install alex happy
```

### Verificar instalação

```bash
ghc --version      # Deve mostrar versão do GHC
alex --version     # Deve mostrar versão do Alex
happy --version    # Deve mostrar versão do Happy
```

---

## Como Compilar

### Opção 1: Usando Makefile (Recomendado)

```bash
make
```

Este comando:
1. Gera `Lexer.hs` a partir de `Lexer.x` usando Alex
2. Gera `Parser.hs` a partir de `Parser.y` usando Happy
3. Compila todos os módulos com GHC
4. Cria o executável `compilador`

### Opção 2: Usando script de build

```bash
chmod +x build.sh
./build.sh
```

### Opção 3: Usando Cabal

```bash
cabal build
cabal run compilador test.ada
```

### Opção 4: Manualmente (passo a passo)

```bash
# 1. Gerar o lexer
alex Lexer.x

# 2. Gerar o parser
happy Parser.y

# 3. Compilar o programa
ghc -dynamic --make Main.hs -o compilador
```

---

## Como Executar

### Ler programa de um ficheiro:

```bash
./compilador test.ada
```

### Ler da entrada padrão:

```bash
./compilador < test.ada
```

ou

```bash
echo 'procedure Main is begin Put_Line("Hello") end Main;' | ./compilador
```

### Executar testes:

```bash
make test
```

---

## Saída do Compilador

O compilador produz **três secções** de output:

### 1. **ABSTRACT SYNTAX TREE**
Representação formatada e legível da AST, incluindo declarações:

```
=== ABSTRACT SYNTAX TREE ===
Program
  Declarations:
    ├─ VarDecl: x : IntegerType
    ├─ VarDecl: y : IntegerType
  Statements:
    ├─ Assignment
      ├─ Variable: x
      └─ IntLit: 10
    ├─ PutLine
      └─ StringLit: "Hello"
```

### 2. **SEMANTIC ANALYSIS**
Resultados da análise semântica:

```
=== SEMANTIC ANALYSIS ===
✓ No semantic errors found
```

Ou em caso de erros:
```
=== SEMANTIC ANALYSIS ===
✗ Semantic errors found:
  Error: Variable 'z' used but not declared
```

### 3. **SYMBOL TABLE**
Tabela de símbolos com todas as declarações:

```
=== SYMBOL TABLE ===
SymbolTable {scopes = [fromList [
  ("x",SymbolInfo {symbolName = "x", symbolType = IntegerType, scopeLevel = 0}),
  ("y",SymbolInfo {symbolName = "y", symbolType = IntegerType, scopeLevel = 0})
]], currentLevel = 0}
```

---

## Exemplos de Programas

### Exemplo 1: Programa com Declarações

```ada
procedure Main is
  x : Integer;
  msg : String;
begin
  x := 10;
  Put_Line("The value is:");
  Put_Line(x)
end Main;
```

### Exemplo 2: Condicional

```ada
procedure Main is
  x : Integer;
begin
  x := 5;
  if x > 0 then
    Put_Line("Positive")
  else
    Put_Line("Not positive");
end Main;
```

### Exemplo 3: Ciclo While

```ada
procedure Main is
  counter : Integer;
begin
  counter := 0;
  while counter < 5 loop
    Put_Line("Counter:");
    Put_Line(counter);
    counter := counter + 1
  end loop;
  Put_Line("Done")
end Main;
```
    Put_Line(counter);
    counter := counter + 1
  end loop;
  Put_Line("Done")
end Main;
```

### Exemplo 4: Expressões Complexas com Declarações

```ada
procedure Main is
  x : Integer;
  y : Integer;
  z : Integer;
  remainder : Integer;
begin
  x := 10;
  y := 20;
  z := (x + y) * 2 - 5;
  
  if z > 50 and x < y then
    Put_Line("Complex condition is true");
  
  remainder := z mod 3;
  Put_Line(remainder)
end Main;
```

### Exemplo 5: Escopos Aninhados

```ada
procedure Main is
  x : Integer;
  y : Integer;
begin
  x := 10;
  begin
    y := 20;
    Put_Line("Inner block");
    x := x + y
  end;
  Put_Line("Outer block")
end Main;
```

---

## Testes Incluídos

### `test.ada`
Programa de teste principal com:
- Declarações de variáveis
- Atribuições simples
- Expressões aritméticas
- Condicionais if-then-else
- Ciclos while
- Blocos aninhados
- Chamadas a Put_Line

### `test_declarations.ada`
Teste básico de declarações:
- Múltiplas declarações de tipos diferentes
- Uso correto de variáveis declaradas

### `test_undeclared.ada`
Teste de erro de variável não declarada:
- Demonstra deteção de uso de variável não declarada
- Deve produzir erro semântico

### `test_redeclaration.ada`
Teste de erro de redeclaração:
- Demonstra deteção de declaração duplicada
- Deve produzir erro semântico

### `test_nested_scope.ada`
Teste de escopos aninhados:
- Blocos begin...end dentro de outros blocos
- Acesso a variáveis de escopos exteriores

### `test_comprehensive.ada`
Teste abrangente com todos os recursos:
- Declarações múltiplas
- Todos os tipos de comandos
- Escopos múltiplos aninhados
- Expressões complexas
- Condicionais if-then-else
- Ciclos while
- Chamadas a Put_Line

### `test_case.ada`
Demonstra suporte a keywords case-insensitive:
- Mistura de UPPERCASE, lowercase e MixedCase
- Testa todas as construções da linguagem
- Valida conformidade com Ada real

Para executar:
```bash
./compilador test.ada
./compilador test_declarations.ada
./compilador test_undeclared.ada    # Deve mostrar erro
./compilador test_redeclaration.ada # Deve mostrar erro
```

---

## Limitações Conhecidas

1. **Análise de tipos básica:**
   - Não verifica compatibilidade de tipos em expressões
   - Pode tentar somar Integer com Boolean na gramática
   - Não detecta variáveis não inicializadas

2. **Escopo de variáveis:**
   - Variáveis de escopos exteriores são visíveis em escopos interiores
   - Não suporta shadowing (redefinição em escopo interno)

3. **Não há geração de código:**
   - O compilador apenas produz a AST e valida semântica
   - Não gera código executável ou código intermédio

4. **Estruturas não suportadas:**
   - Arrays
   - Records (structs)
   - Procedures/functions definidas pelo utilizador
   - Parâmetros
   - Tipos definidos pelo utilizador
   - Constantes

5. **Input limitado:**
   - `Get_Line` é reconhecido mas não tem implementação real
   - Apenas reconhecido como parte da sintaxe

---

## Limpeza de Ficheiros Gerados

Para remover ficheiros gerados durante a compilação:

```bash
make clean
```

Isto remove:
- `Lexer.hs` (gerado pelo Alex)
- `Parser.hs` (gerado pelo Happy)
- Ficheiros objeto (`.o`, `.hi`)
- Executável `compilador`
- Ficheiros de informação do parser (`Parser.info`)

---

## Resolução de Problemas

### Erro: "alex: command not found"
```bash
cabal install alex
export PATH="$HOME/.cabal/bin:$PATH"
```

### Erro: "happy: command not found"
```bash
cabal install happy
export PATH="$HOME/.cabal/bin:$PATH"
```

### Erro: "Parse error at token..."
Verifique se:
- O programa começa com `procedure Main is`
- Termina com `end Main;`
- Todos os comandos têm ponto-e-vírgula (exceto o último antes de `end`)
- Parênteses estão balanceados
- Strings estão entre aspas duplas

### Erro: "Procedure name must be 'Main'..."
O nome da procedure deve ser exatamente `Main` (pode ser qualquer capitalização: `Main`, `MAIN`, `main`) e deve coincidir no início e no fim.

---

## Detalhes Técnicos

### Gramática

O parser implementa uma gramática LL com as seguintes produções principais:

```
Program    → procedure id is begin StmtList end id ;
StmtList   → Stmt | Stmt ; StmtList
Stmt       → id := Expr
           | if Expr then Stmt else Stmt
           | if Expr then Stmt
           | while Expr loop StmtList end loop
           | begin StmtList end
           | put_line ( Expr )
Expr       → Expr or Expr
           | Expr and Expr
           | not Expr
           | CompExpr
CompExpr   → ArithExpr RelOp ArithExpr
           | ArithExpr
ArithExpr  → ArithExpr + ArithExpr
           | ArithExpr - ArithExpr
           | ArithExpr * ArithExpr
           | ArithExpr / ArithExpr
           | ArithExpr mod ArithExpr
           | Term
Term       → int | true | false | string | id
           | get_line | - Term | ( Expr )
```

### Tokens Reconhecidos

O lexer produz tokens do tipo `Token`:
```haskell
data Token = 
    TokenProcedure AlexPosn | TokenIs AlexPosn
  | TokenBegin AlexPosn | TokenEnd AlexPosn
  | TokenIf AlexPosn | TokenThen AlexPosn | TokenElse AlexPosn
  | TokenWhile AlexPosn | TokenLoop AlexPosn
  | TokenAnd AlexPosn | TokenOr AlexPosn | TokenNot AlexPosn
  | TokenMod AlexPosn | TokenTrue AlexPosn | TokenFalse AlexPosn
  | TokenPutLine AlexPosn | TokenGetLine AlexPosn
  | TokenAssign AlexPosn | TokenSemi AlexPosn | TokenColon AlexPosn
  | TokenLParen AlexPosn | TokenRParen AlexPosn
  | TokenPlus AlexPosn | TokenMinus AlexPosn
  | TokenTimes AlexPosn | TokenDiv AlexPosn
  | TokenEq AlexPosn | TokenNeq AlexPosn
  | TokenLt AlexPosn | TokenLte AlexPosn
  | TokenGt AlexPosn | TokenGte AlexPosn
  | TokenInt AlexPosn Int
  | TokenString AlexPosn String
  | TokenId AlexPosn String
```

---

## Referências

- **Ada Language:** https://ada-lang.io
- **Alex User Guide:** https://www.haskell.org/alex/
- **Happy User Guide:** https://www.haskell.org/happy/
- **GHC Documentation:** https://www.haskell.org/ghc/

---

## Notas Finais

Este compilador foi desenvolvido como parte do Trabalho Prático 1 da unidade curricular de Compiladores (DCC-FCUP). Implementa apenas um subconjunto simplificado da linguagem Ada, focado nas fases de análise léxica e sintática.

Para futuras extensões, seria necessário implementar:
- Análise semântica (verificação de tipos, tabela de símbolos)
- Geração de código intermédio
- Otimizações
- Geração de código final (assembly, LLVM, etc.)

---

**Data de Submissão:** 9 de Novembro de 2025  
**Demonstração:** Semana de 10 de Novembro de 2025