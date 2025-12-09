# Compilador Ada - Trabalho Prático 1
**Compiladores - DCC FCUP**  
**Novembro 2025**

## Identificação do Grupo
- **Aluno 1:** [Nome Completo] - [Número de estudante]
- **Aluno 2:** [Nome Completo] - [Número de estudante] 

---

## Descrição do Projeto

Este projeto implementa um **compilador para um subconjunto da linguagem Ada**, conforme especificado no enunciado do Trabalho Prático. O compilador realiza as seguintes fases:

1. **Análise Léxica** (Lexer) - Reconhecimento de tokens
2. **Análise Sintática** (Parser) - Construção da árvore sintática abstrata (AST)
3. **Geração de Código Intermédio** (TAC) - Three-Address Code
4. **Geração de Código MIPS** - Assembly para arquitetura MIPS

O compilador aceita como entrada um programa Ada válido e produz como saída:
- Árvore sintática abstrata (AST) formatada
- Código intermédio em formato TAC (Three-Address Code)
- Código assembly MIPS (.asm)
- Arquivo `.asm` compatível com o simulador MARS

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
begin
  -- comandos aqui
end Main;
```

O programa **deve** ser uma procedure chamada `Main` (case-insensitive), e o nome no início e no fim devem coincidir.

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
data Program = Program [Stmt]

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

### 🔄 Geração de Código Intermédio (TAC)

Implementada no módulo `TAC.hs`, o gerador de TAC converte a AST em código de três endereços:

#### Características:
- **Temporários automáticos:** `t0`, `t1`, `t2`, etc.
- **Labels para controlo de fluxo:** `L0`, `L1`, `L2`, etc.
- **Instruções suportadas:**
  - Atribuições: `x = y`, `x = y op z`
  - Operações: `+`, `-`, `*`, `/`, `mod`, `<`, `>`, `=`, etc.
  - Jumps: `goto L`, `ifz x goto L`
  - Labels: `L0:`

#### Exemplo:
Código Ada:
```ada
x := a + b * c;
```

TAC gerado:
```
  t0 = b * c
  t1 = a + t0
  x = t1
```

---

### 🖥️ Geração de Código MIPS

Implementada no módulo `MIPS.hs`, traduz TAC para assembly MIPS:

#### Características:
- **Alocação de registos:**
  - Variáveis: `$s0`, `$s1`, `$s2`, etc.
  - Temporários: `$t0`, `$t1`, `$t2`, etc.
- **Instruções MIPS:**
  - Aritméticas: `add`, `addi`, `sub`, `mul`, `div`
  - Comparações: `slt`, `slti`, `sgt`, `seq`, `sne`
  - Controlo: `beqz`, `j` (jump)
  - Syscalls: output de strings e inteiros, exit
- **Secção .data:** String literals
- **Optimizações:** Uso de instruções imediatas quando possível

#### Exemplo:
TAC:
```
  t0 = b * c
```

MIPS:
```
  mul $t0, $s1, $s2
```

---

## Estrutura do Projeto

```
.
├── AST.hs              # Definição da AST e TAC
├── TAC.hs              # Geração de código intermédio (Three-Address Code)
├── MIPS.hs             # Geração de código MIPS
├── Lexer.x             # Especificação do lexer (Alex)
├── Parser.y            # Especificação do parser (Happy)
├── Main.hs             # Programa principal
├── Makefile            # Automatização da compilação
├── test.ada            # Programa de teste simples
├── test_arithmetic.ada # Teste de expressões aritméticas
├── test_conditionals.ada # Teste de condicionais
├── test_loop.ada       # Teste de ciclos
├── test_nested.ada     # Teste de estruturas aninhadas
├── test_comprehensive.ada # Teste completo
└── README.md           # Este ficheiro
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

### Compilar um programa Ada:

```bash
./compilador test.ada
```

Isto irá:
1. Analisar o ficheiro `test.ada`
2. Mostrar a AST (Abstract Syntax Tree)
3. Mostrar o TAC (Three-Address Code)
4. Mostrar o código MIPS
5. Gerar o ficheiro `test.asm` com o código MIPS

### Executar código MIPS no MARS:

1. Instale o [MARS MIPS Simulator](http://courses.missouristate.edu/kenvollmar/mars/)
2. Abra o ficheiro `.asm` gerado no MARS
3. Execute o programa (F5)

### Executar testes:

```bash
make test
```

### Ler da entrada padrão:

```bash
./compilador < test.ada
```

ou

```bash
echo 'procedure Main is begin Put_Line("Hello") end Main;' | ./compilador
```

---

## Saída do Compilador

O compilador produz **quatro secções** de output:

### 1. **ABSTRACT SYNTAX TREE**
Representação formatada e legível da AST:

```
=== ABSTRACT SYNTAX TREE ===
Program
  ├─ Assignment
    ├─ Variable: x
    └─ IntLit: 10
  ├─ PutLine
    └─ StringLit: "Hello"
```

### 2. **THREE-ADDRESS CODE**
Código intermédio em formato TAC:

```
=== THREE-ADDRESS CODE ===
  x = 10
  _print = print "Hello"
```

### 3. **MIPS ASSEMBLY CODE**
Código assembly MIPS pronto para executar:

```
=== MIPS ASSEMBLY CODE ===
# Generated MIPS Assembly Code

.data
str0: .asciiz "Hello"

.text
.globl main
main:
  li $s0, 10
  la $a0, str0
  li $v0, 4
  syscall
  li $v0, 10
  syscall
```

### 4. **Ficheiro .asm**
O código MIPS é também guardado num ficheiro `.asm` (e.g., `test.asm`)
=== TOKENS ===
TokenProcedure (line 1, column 1)
TokenId (line 1, column 11) "Main"
TokenIs (line 1, column 16)
...
```

### 2. **ABSTRACT SYNTAX TREE**
Representação formatada e legível da AST:

```
=== ABSTRACT SYNTAX TREE ===
Program:
  x := 10
  Put_Line("Hello")
```

### 3. **HASKELL AST REPRESENTATION**
Representação completa da estrutura de dados em Haskell:

```
=== HASKELL AST REPRESENTATION ===
Program [Assignment "x" (IntLit 10), PutLine (StringLit "Hello")]
```

---

## Exemplos de Programas

### Exemplo 1: Programa Simples

```ada
procedure Main is
begin
  x := 10;
  Put_Line("The value is:");
  Put_Line(x)
end Main;
```

### Exemplo 2: Condicional

```ada
procedure Main is
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

### Exemplo 4: Expressões Complexas

```ada
procedure Main is
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

### Exemplo 5: Exemplo Completo (test_comprehensive.ada)

```ada
procedure Main is
begin
  a := 10;
  b := 20;
  c := a + b * 2;
  
  if c > 30 then
    Put_Line("Large number")
  else
    Put_Line("Small number");
  
  counter := 0;
  factorial := 1;
  
  while counter < 5 loop
    counter := counter + 1;
    factorial := factorial * counter
  end loop;
  
  Put_Line("Factorial of 5 is:");
  Put_Line(factorial);
  
  if factorial > 100 then
    Put_Line("Factorial is greater than 100")
end Main;
```

---

## Testes Incluídos

O projeto inclui vários ficheiros de teste para validar diferentes funcionalidades:

### `test.ada`
Programa de teste básico com:
- Atribuições simples
- Expressões aritméticas
- Condicionais if-then-else
- Ciclos while
- Chamadas a Put_Line

### `test_arithmetic.ada`
Testa operações aritméticas:
- Adição, subtração, multiplicação, divisão
- Operador módulo
- Precedência de operadores

### `test_conditionals.ada`
Testa estruturas condicionais:
- if-then simples
- if-then-else
- Múltiplos condicionais
- Operadores de comparação

### `test_loop.ada`
Testa ciclos while:
- Inicialização de variáveis
- Condição de loop
- Incremento de contador
- Acumulação de valores

### `test_nested.ada`
Testa estruturas aninhadas:
- Blocos dentro de condicionais
- Ciclos dentro de blocos
- Condicionais dentro de ciclos

### `test_comprehensive.ada`
Teste completo que combina:
- Expressões aritméticas complexas
- Múltiplos condicionais
- Ciclos com cálculos
- Output de resultados

Para executar todos os testes:
```bash
./compilador test.ada
./compilador test_arithmetic.ada
./compilador test_conditionals.ada
./compilador test_loop.ada
./compilador test_nested.ada
./compilador test_comprehensive.ada
```

---

## Limitações Conhecidas

1. **Análise semântica limitada:**
   - Não verifica se variáveis foram declaradas antes de serem usadas
   - Não verifica tipos (pode tentar somar string com inteiro na AST)
   - Não detecta variáveis não inicializadas

2. **Sem declaração de variáveis:**
   - O compilador assume que todas as variáveis usadas existem
   - Não há tipos explícitos (Integer, String, Boolean)
   - Todas as variáveis são tratadas como inteiros no MIPS

3. **Alocação de registos simples:**
   - Número limitado de registos disponíveis
   - Não implementa spilling para memória
   - Pode falhar com muitas variáveis simultâneas

4. **Estruturas não suportadas:**
   - Arrays
   - Records (structs)
   - Procedures/functions definidas pelo utilizador
   - Parâmetros
   - Tipos definidos pelo utilizador

5. **Input limitado:**
   - `Get_Line` é reconhecido e gera código MIPS para leitura
   - Usa syscall 5 do MIPS para ler inteiros

6. **Optimizações:**
   - Optimizações limitadas no código gerado
   - Não implementa propagação de constantes
   - Não elimina código morto

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

**Nota:** Os ficheiros `.asm` gerados **não** são removidos pelo `make clean`.
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
- **MARS MIPS Simulator:** http://courses.missouristate.edu/kenvollmar/mars/
- **MIPS Assembly Reference:** https://www.cs.cornell.edu/courses/cs3410/2019sp/schedule/mips-ref.pdf

---

## Notas Finais

Este compilador foi desenvolvido como parte do Trabalho Prático da unidade curricular de Compiladores (DCC-FCUP). Implementa um subconjunto simplificado da linguagem Ada com as seguintes fases:

1. **Análise Léxica** - Tokenização usando Alex
2. **Análise Sintática** - Parsing usando Happy e construção da AST
3. **Geração de Código Intermédio** - Conversão da AST para TAC
4. **Geração de Código Final** - Tradução de TAC para MIPS assembly

O código gerado é compatível com o simulador MARS e pode ser executado para validar a corretude da compilação.

### Melhorias Futuras

Para futuras extensões, seria útil implementar:
- Análise semântica completa (verificação de tipos, tabela de símbolos)
- Optimizações de código (propagação de constantes, eliminação de código morto)
- Alocação de registos mais sofisticada com spilling
- Suporte para arrays e records
- Procedures e functions definidas pelo utilizador
- Geração de código para outras arquiteturas (x86, ARM, LLVM IR)

---

**Data de Submissão:** 9 de Novembro de 2025  
**Demonstração:** Semana de 10 de Novembro de 2025