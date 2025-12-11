# Compiladores - Trabalho Prático 2
**DCC-FCUP - Dezembro 2025**

## Identificação do Grupo
- **Aluno 1:** [Nome Completo] - [Número de estudante]
- **Aluno 2:** [Nome Completo] - [Número de estudante]

---

# PARTE 2: Symbol Table, Three-Address Code e Geração de Código MIPS

## 1. Introdução

Este documento descreve a implementação da segunda parte do trabalho prático, que completa o compilador Ada com:

1. **Tabela de Símbolos** - Gestão de declarações de variáveis e escopos
2. **Análise Semântica** - Validação de uso de variáveis e detecção de erros
3. **Código Intermediário (TAC)** - Representação em Three-Address Code
4. **Geração de Código MIPS** - Tradução de TAC para assembly MIPS

---

## 2. Tabela de Símbolos (Symbol Table)

### 2.1 Estrutura de Dados

A tabela de símbolos foi implementada usando uma estrutura de pilha de escopos:

```haskell
-- Tipos de variáveis suportados
data Type = IntegerType | BooleanType
  deriving (Show, Eq)

-- Informação armazenada para cada símbolo
data SymbolInfo = SymbolInfo 
  { symbolName :: String    -- Nome da variável
  , symbolType :: Type      -- Tipo (Integer ou Boolean)
  , scopeLevel :: Int       -- Nível de escopo (0 = global)
  }
  deriving (Show, Eq)

-- Escopo individual (usa Map para eficiência O(log n))
type Scope = Map.Map String SymbolInfo

-- Tabela de símbolos completa
data SymbolTable = SymbolTable
  { scopes :: [Scope]       -- Pilha de escopos (topo = escopo atual)
  , currentLevel :: Int     -- Nível atual
  }
  deriving (Show, Eq)
```

### 2.2 Operações Principais

#### 2.2.1 Criar Tabela Vazia
```haskell
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Map.empty] 0
```

#### 2.2.2 Entrar em Novo Escopo
```haskell
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes level) = 
  SymbolTable (Map.empty : scopes) (level + 1)
```

Usado quando entramos em blocos `begin...end`.

#### 2.2.3 Sair de Escopo
```haskell
exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable (_:rest) level) = 
  SymbolTable rest (level - 1)
```

Remove o escopo atual quando saímos de um bloco.

#### 2.2.4 Inserir Símbolo
```haskell
insertSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable
insertSymbol name typ (SymbolTable (currentScope:rest) level) =
  if Map.member name currentScope
  then Nothing  -- Já declarada no escopo atual
  else let info = SymbolInfo name typ level
           newScope = Map.insert name info currentScope
       in Just (SymbolTable (newScope:rest) level)
```

Retorna `Nothing` se a variável já foi declarada no escopo atual (erro de redeclaração).

#### 2.2.5 Procurar Símbolo
```haskell
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name (SymbolTable scopes _) = 
  lookupInScopes name scopes
  where
    lookupInScopes _ [] = Nothing
    lookupInScopes n (s:ss) = 
      case Map.lookup n s of
        Just info -> Just info
        Nothing -> lookupInScopes n ss
```

Procura do escopo atual para o global (shadowing permitido).

### 2.3 Integração com AST

A estrutura `Program` foi modificada para incluir declarações:

```haskell
-- Antes (Parte 1)
data Program = Program [Stmt]

-- Agora (Parte 2)
data Program = Program [Decl] [Stmt]

-- Declaração de variável
data Decl = VarDecl String Type
```

---

## 3. Análise Semântica

### 3.1 Estrutura de Resultados

```haskell
data SemanticResult = SemanticResult
  { errors :: [String]        -- Erros semânticos encontrados
  , warnings :: [String]      -- Avisos (não bloqueiam compilação)
  , symbolTable :: SymbolTable -- Tabela de símbolos resultante
  }
```

### 3.2 Análise do Programa

```haskell
analyzeProgram :: Program -> SemanticResult
analyzeProgram (Program decls stmts) =
  let initialST = emptySymbolTable
      (declErrors, stAfterDecls) = processDeclarations decls initialST
      stmtErrors = checkStatements stmts stAfterDecls
  in SemanticResult 
     { errors = declErrors ++ stmtErrors
     , warnings = []
     , symbolTable = stAfterDecls
     }
```

### 3.3 Processamento de Declarações

```haskell
processDeclarations :: [Decl] -> SymbolTable -> ([String], SymbolTable)
processDeclarations [] st = ([], st)
processDeclarations (VarDecl name typ : rest) st =
  case insertSymbol name typ st of
    Nothing -> 
      let (errs, finalST) = processDeclarations rest st
          errorMsg = "Error: Variable '" ++ name ++ "' already declared"
      in (errorMsg : errs, finalST)
    Just newST -> processDeclarations rest newST
```

### 3.4 Validação de Statements

Para cada statement, verifica:
- **Assignments**: Variável no lado esquerdo está declarada
- **Expressões**: Todas as variáveis usadas estão declaradas
- **Blocos**: Gerencia entrada/saída de escopos

### 3.5 Tipos de Erros Detectados

1. **Variável não declarada**
   ```ada
   procedure Main is
     x : Integer;
   begin
     y := 10  -- Erro: y não declarada
   end Main;
   ```

2. **Redeclaração no mesmo escopo**
   ```ada
   procedure Main is
     x : Integer;
     x : Boolean;  -- Erro: x já declarada
   begin
     x := 10
   end Main;
   ```

---

## 4. Three-Address Code (TAC)

### 4.1 Definição das Instruções

```haskell
data TAC =
    Assign String String                   -- x := y
  | BinOp String String String String      -- x := y op z
  | UnOp String String String              -- x := op y
  | Goto String                            -- goto label
  | Ifz String String                      -- ifz x goto label
  | Label String                           -- label:
  deriving (Show, Eq)
```

### 4.2 Estado de Geração

```haskell
data TACState = TACState {
    tempCount :: Int,     -- Contador para temporárias (t0, t1, t2...)
    labelCount :: Int     -- Contador para labels (L0, L1, L2...)
} deriving (Show)
```

### 4.3 Geração de TAC

#### 4.3.1 Expressões Aritméticas

```haskell
generateExpr :: Expr -> TACState -> ([TAC], String, TACState)

-- Literais retornam diretamente
generateExpr (IntLit n) state = ([], show n, state)

-- Operações binárias geram temporária
generateExpr (Add e1 e2) state =
    let (tac1, res1, state1) = generateExpr e1 state
        (tac2, res2, state2) = generateExpr e2 state1
        (temp, state3) = newTemp state2
    in (tac1 ++ tac2 ++ [BinOp temp res1 res2 "Add"], temp, state3)
```

**Exemplo:**
```ada
x := 5 + 3 * 2
```

Gera TAC:
```
t0 = 3 * 2
t1 = 5 + t0
x = t1
```

#### 4.3.2 Estruturas de Controle

**If-Then-Else:**
```haskell
generateStmt (IfThenElse cond thenStmt elseStmt) state =
    let (condTAC, condResult, state1) = generateExpr cond state
        (elseLbl, state2) = newLabel state1
        (endLbl, state3) = newLabel state2
        (thenTAC, state4) = generateStmt thenStmt state3
        (elseTAC, state5) = generateStmt elseStmt state4
    in (condTAC ++ 
        [Ifz condResult elseLbl] ++
        thenTAC ++
        [Goto endLbl, Label elseLbl] ++
        elseTAC ++
        [Label endLbl], state5)
```

**Exemplo:**
```ada
if x > 10 then
  y := 1
else
  y := 2
```

Gera TAC:
```
t0 = x > 10
ifz t0 goto L0
y = 1
goto L1
L0:
y = 2
L1:
```

**While Loop:**
```haskell
generateStmt (While cond body) state =
    let (startLbl, state1) = newLabel state
        (endLbl, state2) = newLabel state1
        (condTAC, condResult, state3) = generateExpr cond state2
        (bodyTAC, state4) = generateStmt body state3
    in ([Label startLbl] ++
        condTAC ++
        [Ifz condResult endLbl] ++
        bodyTAC ++
        [Goto startLbl, Label endLbl], state4)
```

**Exemplo:**
```ada
while counter < 5 loop
  counter := counter + 1
end loop
```

Gera TAC:
```
L0:
t0 = counter < 5
ifz t0 goto L1
t1 = counter + 1
counter = t1
goto L0
L1:
```

### 4.4 Pretty Printing

```haskell
prettyPrintTAC :: [TAC] -> String
prettyPrintTAC tacs = intercalate "\n" (map prettyPrintTACInstr tacs)

prettyPrintTACInstr :: TAC -> String
prettyPrintTACInstr (Assign dest src) = "  " ++ dest ++ " = " ++ src
prettyPrintTACInstr (BinOp dest src1 src2 op) = 
    "  " ++ dest ++ " = " ++ src1 ++ " " ++ opSymbol op ++ " " ++ src2
prettyPrintTACInstr (Label lbl) = lbl ++ ":"
-- etc...
```

---

## 5. Geração de Código MIPS

### 5.1 Estrutura de Estado

```haskell
data MIPSState = MIPSState {
    varMap :: Map.Map String String,      -- Variável -> Registrador/Memória
    nextStackOffset :: Int,               -- Próximo offset na stack
    stringLiterals :: [(String, String)], -- Strings e seus labels
    nextVarReg :: Int                     -- Próximo $s disponível
}
```

### 5.2 Alocação de Registradores

#### 5.2.1 Estratégia de Alocação

- **$s0 - $s7**: 8 registradores para variáveis permanentes
- **$t0 - $t9**: 10 registradores para temporárias
- **Stack**: Usado quando $s0-$s7 estão esgotados

```haskell
allocateVar st var = 
    if Map.member var (varMap st)
    then st
    else if nextVarReg st < 8
         then let reg = "$s" ++ show (nextVarReg st)
                  newMap = Map.insert var reg (varMap st)
              in st { varMap = newMap, nextVarReg = nextVarReg st + 1 }
         else let offset = nextStackOffset st + 4
                  memLoc = show offset ++ "($sp)"
                  newMap = Map.insert var memLoc (varMap st)
              in st { varMap = newMap, nextStackOffset = offset }
```

### 5.3 Tradução de Instruções TAC

#### 5.3.1 Atribuições Simples

```haskell
-- TAC: x = 10
-- MIPS: li $s0, 10

-- TAC: x = y
-- MIPS: move $s0, $s1
```

#### 5.3.2 Operações Binárias

```haskell
-- TAC: t0 = x + y
-- MIPS: add $t0, $s0, $s1

-- TAC: t0 = x * 5
-- MIPS: li $t1, 5
--       mul $t0, $s0, $t1
```

#### 5.3.3 Controle de Fluxo

```haskell
-- TAC: ifz t0 goto L1
-- MIPS: beqz $t0, L1

-- TAC: goto L2
-- MIPS: j L2

-- TAC: L0:
-- MIPS: L0:
```

### 5.4 Seção de Dados

```haskell
extractStrings :: [TAC] -> MIPSState -> (String, MIPSState)
```

Gera:
```mips
.data
str0: .asciiz "Hello World"
str1: .asciiz "Result:"
newline: .asciiz "\n"
```

### 5.5 Estrutura do Código Gerado

```mips
# Generated MIPS Assembly Code

.data
str0: .asciiz "String literal"
newline: .asciiz "\n"

.text
.globl main
main:
  # Código gerado aqui
  
  # Exit program
  li $v0, 10
  syscall
```

### 5.6 Syscalls MIPS Usadas

| Syscall | $v0 | Função | Parâmetros |
|---------|-----|--------|------------|
| print_int | 1 | Imprimir inteiro | $a0 = valor |
| print_string | 4 | Imprimir string | $a0 = endereço |
| read_int | 5 | Ler inteiro | resultado em $v0 |
| exit | 10 | Terminar programa | - |

### 5.7 Exemplo Completo

**Código Ada:**
```ada
procedure Main is
  x : Integer;
  y : Integer;
begin
  x := 10;
  y := x + 5;
  Put_Line(y)
end Main;
```

**TAC Gerado:**
```
  x = 10
  t0 = x + 5
  y = t0
  _print = print y
```

**MIPS Gerado:**
```mips
.text
.globl main
main:
  li $s0, 10
  addi $t0, $s0, 5
  move $s1, $t0
  move $a0, $s1
  li $v0, 1
  syscall
  
  li $v0, 10
  syscall
```

---

## 6. Pipeline de Compilação Completo

### 6.1 Fluxo Geral

```
Código Ada (.ada)
    ↓
Análise Léxica (Lexer.x)
    ↓
Análise Sintática (Parser.y)
    ↓
AST (AST.hs)
    ↓
Análise Semântica (Semantic.hs) ← PARTE 2
    ↓
Geração TAC (TAC.hs) ← PARTE 2
    ↓
Geração MIPS (MIPS.hs) ← PARTE 2
    ↓
Assembly MIPS (.asm)
```

### 6.2 Implementação em Main.hs

```haskell
main :: IO ()
main = do
  args <- getArgs
  input <- readFile filename
  
  -- Análise léxica
  let tokens = alexScanTokens input
  
  -- Análise sintática
  let ast = parse tokens
  
  -- Análise semântica (PARTE 2)
  let semanticResult = analyzeProgram ast
  
  if null (errors semanticResult)
    then do
      -- Geração TAC (PARTE 2)
      let tac = generateTAC ast
      
      -- Geração MIPS (PARTE 2)
      let mipsCode = generateMIPS tac
      
      -- Escrever arquivo
      writeFile outputFile mipsCode
    else do
      mapM_ putStrLn (errors semanticResult)
      exitFailure
```

---

## 7. Testes

### 7.1 Casos de Teste Implementados

| Teste | Descrição | Resultado Esperado |
|-------|-----------|-------------------|
| test_declarations.ada | Declarações básicas | Compilação OK |
| test_undeclared.ada | Variável não declarada | Erro semântico |
| test_redeclaration.ada | Redeclaração | Erro semântico |
| test_nested_scope.ada | Escopos aninhados | Compilação OK |
| test_arithmetic_decl.ada | Operações aritméticas | TAC e MIPS corretos |
| test_comprehensive.ada | Programa complexo | Tudo funciona |
| demo_integration.ada | Demonstração completa | Output correto |

### 7.2 Script de Testes

```bash
#!/bin/bash
# run_tests.sh

PASSED=0
FAILED=0

run_test() {
    local test_name="$1"
    local test_file="$2"
    local should_fail="$3"
    
    if [ "$should_fail" = "fail" ]; then
        if ./compilador "$test_file" 2>&1 | grep -q "Semantic errors"; then
            echo "PASS (error detected)"
            PASSED=$((PASSED + 1))
        else
            echo "FAIL"
            FAILED=$((FAILED + 1))
        fi
    else
        if ./compilador "$test_file" > /dev/null 2>&1; then
            echo "PASS"
            PASSED=$((PASSED + 1))
        else
            echo "FAIL"
            FAILED=$((FAILED + 1))
        fi
    fi
}

# Executar testes
run_test "Declarations" "test_declarations.ada" "pass"
run_test "Undeclared variable" "test_undeclared.ada" "fail"
# ... mais testes
```

**Resultado:**
```
==========================================
Integrated Compiler Test Suite
==========================================

=== Semantic Analysis Tests ===
Testing Declarations... PASS
Testing Undeclared variable... PASS (error detected as expected)
Testing Redeclaration... PASS (error detected as expected)
Testing Nested scope... PASS

=== Code Generation Tests ===
Testing Arithmetic operations... PASS
Testing Comprehensive test... PASS
Testing Complex nested test... PASS

==========================================
Test Summary:
  Passed: 7
  Failed: 0
==========================================
All tests passed! ✓
```

---

## 8. Como Usar

### 8.1 Compilação do Compilador

```bash
# Método 1: Usando GHC diretamente
ghc -dynamic --make Main.hs -o compilador

# Método 2: Usando Makefile (requer alex e happy)
make clean
make
```

### 8.2 Uso do Compilador

```bash
# Compilar programa Ada
./compilador programa.ada

# Isso gera:
# - programa.asm (código MIPS)
# - Output no terminal mostrando AST, TAC, MIPS
```

### 8.3 Executar Código MIPS

Use o simulador MARS:
```bash
java -jar Mars.jar programa.asm
```

---

## 9. Estrutura de Arquivos

```
Compilador/
├── AST.hs                    # AST + Symbol Table + TAC types
├── Semantic.hs               # Análise semântica
├── TAC.hs                    # Geração TAC
├── MIPS.hs                   # Geração MIPS
├── Main.hs                   # Driver principal
├── Lexer.x                   # Especificação léxico
├── Parser.y                  # Especificação sintático
├── Token.hs                  # Definição de tokens
├── Makefile                  # Build system
├── build.sh                  # Script de build
├── run_tests.sh              # Script de testes
├── test_*.ada                # Casos de teste
├── INTEGRATION.md            # Documentação técnica
├── README_INTEGRATION.md     # Guia do usuário
├── SUMMARY.md                # Relatório completo
└── README.pdf                # Este documento
```

---

## 10. Decisões de Implementação

### 10.1 Symbol Table

**Decisão:** Usar `Map.Map` em vez de listas.
**Razão:** Lookup O(log n) vs O(n), importante para programas grandes.

### 10.2 TAC

**Decisão:** Ignorar declarações na geração TAC.
**Razão:** Declarações são processadas na análise semântica e não geram código.

### 10.3 MIPS

**Decisão:** Alocação simples de registradores sem otimização.
**Razão:** Simplicidade e clareza. Registradores $s para variáveis, $t para temporárias.

**Decisão:** Spilling para stack quando $s0-$s7 esgotam.
**Razão:** Suportar programas com mais de 8 variáveis.

### 10.4 Erros Semânticos

**Decisão:** Continuar análise após erro para reportar múltiplos erros.
**Razão:** Melhor experiência do usuário - ver todos os erros de uma vez.

---

## 11. Limitações e Trabalho Futuro

### 11.1 Limitações Atuais

1. **Tipos:** Apenas Integer e Boolean (sem Float, String como tipo)
2. **Arrays:** Não suportados
3. **Funções:** Não suportadas (apenas procedure Main)
4. **Type Checking:** Não valida tipos em expressões
5. **Otimizações:** Nenhuma otimização de código

### 11.2 Possíveis Melhorias

1. **Type Checking Completo**
   - Validar tipos em operações
   - Detectar incompatibilidades de tipo

2. **Otimizações TAC**
   - Constant folding
   - Dead code elimination
   - Common subexpression elimination

3. **Otimizações MIPS**
   - Register allocation avançado (graph coloring)
   - Peephole optimizations
   - Instruction scheduling

4. **Features Adicionais**
   - Suporte a arrays
   - Procedures e functions com parâmetros
   - Records/structs

---

## 12. Conclusão

A segunda parte do trabalho implementa com sucesso:

✅ **Tabela de Símbolos** completa com gestão de escopos
✅ **Análise Semântica** detectando erros de declaração e uso
✅ **Geração TAC** como representação intermediária
✅ **Geração MIPS** traduzindo TAC para assembly executável

O compilador agora oferece um pipeline completo de compilação, desde código fonte Ada até assembly MIPS executável no simulador MARS, com validação semântica garantindo correção antes da geração de código.

**Todos os testes passam (7/7)** e o código está documentado e pronto para uso.

---

## 13. Referências

1. **Dragon Book** - Compilers: Principles, Techniques, and Tools (Aho, Sethi, Ullman)
2. **MIPS Assembly Language** - Patterson & Hennessy
3. **MARS Simulator** - https://github.com/dpetersanderson/MARS
4. **Haskell Documentation** - https://www.haskell.org/documentation/
5. **Alex Manual** - https://www.haskell.org/alex/
6. **Happy Manual** - https://www.haskell.org/happy/

---

**Fim do Documento**
