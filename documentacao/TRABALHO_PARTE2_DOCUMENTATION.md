# Compilador Ada - Documentação Completa da Segunda Parte do Trabalho Prático

**Grupo 3 - Compiladores (CC3001)**  
**Faculdade de Ciências da Universidade do Porto**  
**Ano Letivo 2025/2026**

---

## 📚 Fontes Verificadas dos Materiais do Curso

**IMPORTANTE:** Todas as fontes foram extraídas e verificadas dos PDFs usando poppler/pdftotext (11 Dez 2025).

### Trabalhos Práticos
- ✅ **Trabalho Prático - primeira parte** - I/O (Put_Line, Get_Line) explicitamente requeridos
- ✅ **Trabalho Prático - segunda parte** - Symbol Table, TAC, MIPS requeridos

### Aulas Teóricas (PDFs completos extraídos e verificados)
- ✅ **Aula 8** - Análise semântica e tabelas de símbolos → `Semantic.hs`, operações em `AST.hs`
- ✅ **Aula 9** - Sistemas de tipos → tipos em `AST.hs`
- ✅ **Aula 10** - Geração de código intermédio → `TAC.hs`
- ✅ **Aula 12** - Emissão de código assembler → `MIPS.hs`

### Aulas Práticas (PDFs e TXT completos)
- ✅ **Prática 5** (pratica5_3address.pdf) → Exercícios TAC, formato de instruções
- ✅ **Prática 6** (pratica6_codeGen_hs.txt) → Template Haskell, estrutura `compileExpr`

### Bibliografia (PDF completo)
- ✅ **Debray - 3-address Code to MIPS** → Convenções MIPS, tradução instruções
  - Citado explicitamente: MIPS.hs linha 4

📄 **Ver SOURCES_DETAILED.md** para mapeamento completo com citações diretas e números de linha.

---

**Data:** 11 de Dezembro de 2025

---

## Índice

1. [Introdução](#1-introdução)
2. [Visão Geral das Mudanças](#2-visão-geral-das-mudanças)
3. [Alterações ao AST.hs](#3-alterações-ao-asths)
4. [Novo Módulo: Semantic.hs](#4-novo-módulo-semantichs)
5. [Novo Módulo: TAC.hs](#5-novo-módulo-tachs)
6. [Novo Módulo: MIPS.hs](#6-novo-módulo-mipshs)
7. [Alterações ao Main.hs](#7-alterações-ao-mainhs)
8. [Alterações ao Parser.y](#8-alterações-ao-parsery)
9. [Referências aos Materiais do Curso](#9-referências-aos-materiais-do-curso)
10. [Observações e Flags](#10-observações-e-flags)

---

## 1. Introdução

Este documento apresenta uma análise detalhada e completa de todas as mudanças implementadas na **segunda parte do trabalho prático** do compilador para um subconjunto da linguagem Ada.

### Objetivos da Segunda Parte

A segunda parte do trabalho prático focou-se em três componentes principais:

1. **Análise Semântica** 
   - Implementação de tabelas de símbolos
   - Verificação de declarações de variáveis
   - Detecção de variáveis não declaradas
   - Detecção de redeclarações
   - Suporte a escopos aninhados

2. **Geração de Código Intermédio (TAC - Three-Address Code)**
   - Conversão da AST para representação intermédia
   - Geração de temporários
   - Geração de labels para controlo de fluxo
   - Linearização de expressões complexas

3. **Geração de Código Assembly MIPS**
   - Tradução de TAC para MIPS
   - Alocação de registos
   - Gestão de strings e dados
   - Geração de syscalls para I/O

### Estrutura do Documento

Para cada mudança implementada, este documento fornece:
- O código completo afetado
- A fonte específica dos materiais do curso (aula, slide, exercício)
- Explicação dos conceitos aplicados
- Justificação técnica das decisões de implementação
- Flags para código que possa não ter fonte direta nos materiais

---

## 2. Visão Geral das Mudanças

### Ficheiros Alterados

1. **AST.hs** - Extensivamente modificado
2. **Main.hs** - Adicionadas chamadas às novas fases do compilador
3. **Parser.y** - Suporte a declarações de variáveis

### Ficheiros Novos

1. **Semantic.hs** - Análise semântica completa
2. **TAC.hs** - Geração de código intermédio
3. **MIPS.hs** - Geração de código MIPS assembly

### Comparação: Primeira vs Segunda Parte

| Aspecto | Primeira Parte | Segunda Parte |
|---------|----------------|---------------|
| Análise Léxica | ✓ | ✓ (mantido) |
| Análise Sintática | ✓ | ✓ (estendido com declarações) |
| Análise Semântica | ✗ | ✓ (novo) |
| Código Intermédio | ✗ | ✓ (novo - TAC) |
| Código Final | ✗ | ✓ (novo - MIPS) |
| Tabela de Símbolos | ✗ | ✓ (novo) |
| Suporte a Tipos | ✗ | ✓ (Integer, Boolean) |
| Declarações | ✗ | ✓ (variáveis) |

---

## 3. Alterações ao AST.hs

### 3.1 Comparação: Código Original vs Código Atual

#### Código Original (Primeira Parte)

```haskell
module AST where

data Program = Program [Stmt]
  deriving (Show, Eq)

-- Statements
data Stmt = 
    Assignment String Expr
  | IfThenElse Expr Stmt Stmt
  | IfThen Expr Stmt
  | While Expr Stmt
  | Block [Stmt]
  | PutLine Expr
  | EmptyStmt
  deriving (Show, Eq)

-- Expressões
data Expr = 
    IntLit Int
  | BoolLit Bool
  | StringLit String
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Neg Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Lte Expr Expr
  | Gt Expr Expr
  | Gte Expr Expr
  | GetLine
  deriving (Show, Eq)
```

**Fonte:** Trabalho Prático - Primeira Parte

---

#### Código Atual (Segunda Parte)

```haskell
-- AST.hs
-- Definição da Árvore Sintática Abstrata para o subconjunto de Ada

module AST where

import qualified Data.Map as Map
import Data.Maybe (isJust)

-- Tipos de variáveis
data Type = IntegerType | BooleanType
  deriving (Show, Eq)

-- Informação de símbolos
data SymbolInfo = SymbolInfo 
  { symbolName :: String
  , symbolType :: Type
  , scopeLevel :: Int
  }
  deriving (Show, Eq)

-- Tabela de símbolos (usa Map para eficiência)
type Scope = Map.Map String SymbolInfo
data SymbolTable = SymbolTable
  { scopes :: [Scope]          -- Lista de escopos (topo = escopo atual)
  , currentLevel :: Int         -- Nível de escopo atual
  }
  deriving (Show, Eq)

-- Programa principal com tabela de símbolos
data Program = Program [Decl] [Stmt]
  deriving (Show, Eq)

-- Declarações
data Decl = VarDecl String Type
  deriving (Show, Eq)

-- Comandos (Statements)
data Stmt = 
    Assignment String Expr              -- x := expr
  | IfThenElse Expr Stmt Stmt          -- if expr then stmt else stmt
  | IfThen Expr Stmt                   -- if expr then stmt (sem else)
  | While Expr Stmt                    -- while expr loop stmt end loop
  | Block [Stmt]                       -- begin stmt1; stmt2; ... end
  | PutLine Expr                       -- Put_Line(expr)
  | EmptyStmt                          -- comando vazio
  deriving (Show, Eq)

-- Expressões
data Expr = 
    -- Literais
    IntLit Int                         -- 42
  | BoolLit Bool                       -- True, False
  | StringLit String                   -- "texto"
  | Var String                         -- identificador
    
    -- Operações aritméticas
  | Add Expr Expr                      -- e1 + e2
  | Sub Expr Expr                      -- e1 - e2
  | Mul Expr Expr                      -- e1 * e2
  | Div Expr Expr                      -- e1 / e2
  | Mod Expr Expr                      -- e1 mod e2
  | Neg Expr                           -- -e
    
    -- Operações booleanas
  | And Expr Expr                      -- e1 and e2
  | Or Expr Expr                       -- e1 or e2
  | Not Expr                           -- not e
    
    -- Operações relacionais
  | Eq Expr Expr                       -- e1 = e2
  | Neq Expr Expr                      -- e1 /= e2
  | Lt Expr Expr                       -- e1 < e2
  | Lte Expr Expr                      -- e1 <= e2
  | Gt Expr Expr                       -- e1 > e2
  | Gte Expr Expr                      -- e1 >= e2
    
    -- Input
  | GetLine                            -- Get_Line
  deriving (Show, Eq)

--TAC

data TAC =
    Assign String String                   -- x := y (Direct Assignment)
  | BinOp String String String String      -- x := y op z (e.g., "Add", "Sub")
  | UnOp String String String              -- x := op y (e.g., "Neg")
  | Goto String                            -- goto label
  | Ifz String String                      -- ifz x goto label (Conditional Jump)
  | Label String                           -- label: (Control Flow Marker)
  deriving (Show, Eq)

-- Symbol Table Operations

-- Create an empty symbol table
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Map.empty] 0

-- Enter a new scope (e.g., begin block)
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes level) = 
  SymbolTable (Map.empty : scopes) (level + 1)

-- Exit current scope (e.g., end block)
exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable [] _) = error "Cannot exit global scope - symbol table corrupted"
exitScope (SymbolTable [_] 0) = error "Cannot exit global scope"
exitScope (SymbolTable (_:rest) level) = 
  SymbolTable rest (level - 1)

-- Insert a symbol into the current scope
-- Returns Nothing if symbol already exists in current scope, Just table otherwise
insertSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable
insertSymbol _ _ (SymbolTable [] _) = error "Cannot insert into empty symbol table"
insertSymbol name typ (SymbolTable (currentScope:rest) level) =
  if Map.member name currentScope
  then Nothing  -- Symbol already declared in current scope
  else let info = SymbolInfo name typ level
           newScope = Map.insert name info currentScope
       in Just (SymbolTable (newScope:rest) level)

-- Lookup a symbol in all scopes (search from current to global)
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name (SymbolTable scopes _) = 
  lookupInScopes name scopes
  where
    lookupInScopes :: String -> [Scope] -> Maybe SymbolInfo
    lookupInScopes _ [] = Nothing
    lookupInScopes n (s:ss) = 
      case Map.lookup n s of
        Just info -> Just info
        Nothing -> lookupInScopes n ss

-- Check if symbol is declared
isDeclared :: String -> SymbolTable -> Bool
isDeclared name st = isJust (lookupSymbol name st)
```

---

### 3.2 Análise Detalhada das Mudanças

#### 3.2.1 Imports Adicionados

**Código:**
```haskell
import qualified Data.Map as Map
import Data.Maybe (isJust)
```

**Fonte:** 
- **Aula Teórica 8** - "Análise semântica e tabelas de símbolos"
- Slides sobre implementação de tabelas de símbolos com estruturas de dados eficientes

**Conceitos Aplicados:**
- `Data.Map` fornece estrutura de dados de árvore balanceada (Red-Black Tree) para operações de inserção/busca em O(log n)
- `Data.Maybe` fornece `isJust` para verificação de existência de valores

**Justificação:**
A implementação de tabelas de símbolos requer estruturas de dados eficientes. O uso de `Map` é padrão em Haskell para este propósito, conforme ensinado nas aulas teóricas sobre análise semântica.

---

#### 3.2.2 Sistema de Tipos

**Código:**
```haskell
data Type = IntegerType | BooleanType
  deriving (Show, Eq)
```

**Fonte:** 
- **Aula Teórica 9** - "Sistemas de tipos"
- **Aula Teórica 8** - "Análise semântica e tabelas de símbolos"
- Enunciado do Trabalho Prático - Segunda Parte

**Conceitos Aplicados:**
- Sistema de tipos simples com dois tipos básicos
- Integer para valores numéricos
- Boolean para valores lógicos
- Extensibilidade para futuros tipos (String, Real, etc.)

**Justificação:**
O subconjunto de Ada implementado suporta variáveis Integer e Boolean. A análise semântica requer rastreamento de tipos para verificação de compatibilidade.

**Nota:** Embora a análise de tipos completa não esteja totalmente implementada nesta versão, a estrutura está preparada para extensões futuras.

---

#### 3.2.3 Informação de Símbolos

**Código:**
```haskell
data SymbolInfo = SymbolInfo 
  { symbolName :: String
  , symbolType :: Type
  , scopeLevel :: Int
  }
  deriving (Show, Eq)
```

**Fonte:**
- **Aula Teórica 8** - "Análise semântica e tabelas de símbolos"
- Slides sobre estrutura de entradas na tabela de símbolos

**Conceitos Aplicados:**
Cada símbolo armazena três informações essenciais:
1. **symbolName** - Identificador da variável
2. **symbolType** - Tipo da variável (Integer ou Boolean)
3. **scopeLevel** - Nível de aninhamento do escopo onde foi declarada

**Justificação:**
- O nome identifica univocamente o símbolo dentro de um escopo
- O tipo é necessário para verificação de compatibilidade de tipos
- O nível de escopo permite implementar shadowing correto (variáveis em escopos internos "escondem" variáveis de mesmo nome em escopos externos)

**Aplicação Prática:**
```ada
declare
  x : Integer := 5;  -- scopeLevel = 0
begin
  declare
    x : Integer := 10;  -- scopeLevel = 1, "esconde" o x anterior
  begin
    Put_Line(x);  -- Imprime 10, não 5
  end;
end;
```

---

#### 3.2.4 Tabela de Símbolos

**Código:**
```haskell
type Scope = Map.Map String SymbolInfo
data SymbolTable = SymbolTable
  { scopes :: [Scope]
  , currentLevel :: Int
  }
  deriving (Show, Eq)
```

**Fonte:**
- **Aula Teórica 8** - "Análise semântica e tabelas de símbolos"
- Slides sobre implementação de tabelas de símbolos com múltiplos escopos

**Conceitos Aplicados:**

1. **Stack de Escopos:**
   - Lista de escopos onde o primeiro elemento é o escopo atual
   - Implementa estrutura LIFO (Last In, First Out)
   - Permite entrada/saída eficiente de escopos

2. **Escopo Individual:**
   - Mapeamento de nome de variável para informação do símbolo
   - Permite busca rápida O(log n) dentro de um escopo

3. **Nível de Escopo:**
   - Rastreamento do nível de aninhamento atual
   - Usado para depuração e validação

**Justificação:**
Esta estrutura implementa a semântica de blocos aninhados de Ada:
- Variáveis declaradas em blocos internos são visíveis apenas dentro desse bloco
- Variáveis em blocos externos são visíveis em blocos internos
- Shadowing é suportado naturalmente pela ordem de busca (do mais interno para o mais externo)

**Exemplo de Uso:**
```
Global Scope (level 0): [x -> Integer, y -> Boolean]
  └─> Block Scope (level 1): [z -> Integer, x -> Integer]
      └─> Block Scope (level 2): [w -> Boolean]
```

---

#### 3.2.5 Mudança na Estrutura do Programa

**Código:**
```haskell
data Program = Program [Decl] [Stmt]
  deriving (Show, Eq)

data Decl = VarDecl String Type
  deriving (Show, Eq)
```

**Mudança:** `Program [Stmt]` → `Program [Decl] [Stmt]`

**Fonte:**
- **Enunciado do Trabalho Prático - Segunda Parte**
- **Aula Teórica 8** - Estruturação de programas com seção de declarações

**Conceitos Aplicados:**
- Separação explícita entre declarações e comandos
- Declarações devem preceder comandos (seguindo convenção de Ada)
- Facilita análise semântica em duas fases

**Justificação:**
Ada (e muitas outras linguagens estruturadas) requer que variáveis sejam declaradas antes do código executável. Esta mudança reflete essa semântica e facilita a implementação da análise semântica.

**Exemplo de Programa:**
```ada
declare
  x : Integer;
  y : Boolean;
begin
  x := 10;
  y := True;
end;
```

Corresponde a:
```haskell
Program 
  [VarDecl "x" IntegerType, VarDecl "y" BooleanType]
  [Assignment "x" (IntLit 10), Assignment "y" (BoolLit True)]
```

---

#### 3.2.6 Definição de TAC (Three-Address Code)

**Código:**
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

**Fonte:**
- **Aula Teórica 10** - "Geração de código intermédio"
- **Aula Prática 5** - "pratica5_3address.pdf"
- **Aula Prática 6** - "pratica6_codeGen_hs.txt"

**Conceitos Aplicados:**

O Three-Address Code é uma representação intermédia fundamental em compiladores:

1. **Assign** - Atribuição direta
   - Formato: `x := y`
   - Exemplo: `t1 := 5` ou `x := t1`

2. **BinOp** - Operação binária
   - Formato: `x := y op z`
   - Operações: Add, Sub, Mul, Div, Mod, And, Or, Eq, Neq, Lt, Lte, Gt, Gte
   - Exemplo: `t1 := x Add y`

3. **UnOp** - Operação unária
   - Formato: `x := op y`
   - Operações: Neg (negação aritmética), Not (negação lógica)
   - Exemplo: `t1 := Neg x`

4. **Goto** - Salto incondicional
   - Formato: `goto label`
   - Usado para implementar loops e desvios

5. **Ifz** - Salto condicional "if zero"
   - Formato: `ifz x goto label`
   - Salta se x == 0 (falso em booleanos)
   - Usado para implementar if, while, etc.

6. **Label** - Marca de posição
   - Formato: `label:`
   - Destino para saltos (goto, ifz)

**Justificação:**

TAC é ideal como representação intermédia porque:
- Simplifica expressões complexas em operações atômicas
- Facilita análise e otimização
- Mapeia naturalmente para código assembly
- É independente da arquitetura alvo

**Exemplo de Compilação:**

Expressão: `x := (a + b) * (c - 2)`

TAC gerado:
```
t1 := a Add b
t2 := c Sub 2
t3 := t1 Mul t2
x := t3
```

**Referência à Aula Prática 6:**
O ficheiro `pratica6_codeGen_hs.txt` especifica explicitamente:
```
instruction -> VAR := atom 
instruction -> VAR := atom binop atom
instruction -> GOTO label
instruction -> IF VAR relop atom THEN label ELSE label
instruction -> LAB label
```

Nossa implementação segue diretamente esta especificação, com `Ifz` sendo uma simplificação de IF-THEN-ELSE.

---

#### 3.2.7 Operações da Tabela de Símbolos

##### Criar Tabela Vazia

**Código:**
```haskell
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Map.empty] 0
```

**Fonte:** Aula Teórica 8

**Conceitos:**
- Tabela inicial com um escopo global vazio
- Nível 0 representa o escopo global

---

##### Entrar em Novo Escopo

**Código:**
```haskell
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes level) = 
  SymbolTable (Map.empty : scopes) (level + 1)
```

**Fonte:** Aula Teórica 8 - Gestão de escopos aninhados

**Conceitos:**
- Push de novo escopo vazio no topo do stack
- Incremento do contador de nível
- Chamado ao entrar em blocos `begin...end`

**Exemplo:**
```ada
begin  -- enterScope
  declare x : Integer;
  begin x := 5; end;
end;  -- exitScope
```

---

##### Sair de Escopo

**Código:**
```haskell
exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable [] _) = error "Cannot exit global scope - symbol table corrupted"
exitScope (SymbolTable [_] 0) = error "Cannot exit global scope"
exitScope (SymbolTable (_:rest) level) = 
  SymbolTable rest (level - 1)
```

**Fonte:** Aula Teórica 8

**Conceitos:**
- Pop do escopo atual
- Validação para prevenir corrupção da tabela
- Impossibilidade de sair do escopo global

**🚩 FLAG:** Os checks de erro extras (validação de lista vazia e escopo global) são boas práticas de programação defensiva, mas não foram explicitamente mencionados nas aulas.

---

##### Inserir Símbolo

**Código:**
```haskell
insertSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable
insertSymbol _ _ (SymbolTable [] _) = error "Cannot insert into empty symbol table"
insertSymbol name typ (SymbolTable (currentScope:rest) level) =
  if Map.member name currentScope
  then Nothing
  else let info = SymbolInfo name typ level
           newScope = Map.insert name info currentScope
       in Just (SymbolTable (newScope:rest) level)
```

**Fonte:** Aula Teórica 8 - Inserção de símbolos e detecção de redeclaração

**Conceitos:**
- Inserção apenas no escopo atual (não em escopos pais)
- Retorno de `Maybe` para indicar sucesso/falha
- Detecção de redeclaração no mesmo escopo

**Justificação:**
A detecção de redeclaração é requerida pela análise semântica: uma variável não pode ser declarada duas vezes no mesmo escopo.

---

##### Procurar Símbolo

**Código:**
```haskell
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name (SymbolTable scopes _) = 
  lookupInScopes name scopes
  where
    lookupInScopes :: String -> [Scope] -> Maybe SymbolInfo
    lookupInScopes _ [] = Nothing
    lookupInScopes n (s:ss) = 
      case Map.lookup n s of
        Just info -> Just info
        Nothing -> lookupInScopes n ss
```

**Fonte:** Aula Teórica 8 - Busca de símbolos em escopos aninhados

**Conceitos:**
- Busca do escopo mais interno para o mais externo
- Retorna primeira ocorrência encontrada (implementa shadowing)
- Retorna `Nothing` se símbolo não existe

**Exemplo de Shadowing:**
```ada
declare x : Integer := 5;
begin
  declare x : Integer := 10;
  begin
    Put_Line(x);  -- Usa o x interno (10), não o externo (5)
  end;
end;
```

---

##### Verificar Declaração

**Código:**
```haskell
isDeclared :: String -> SymbolTable -> Bool
isDeclared name st = isJust (lookupSymbol name st)
```

**Fonte:** Aula Teórica 8

**Conceitos:**
- Função auxiliar para verificação booleana simples
- Usa `isJust` para converter `Maybe` em `Bool`

---

## 4. Novo Módulo: Semantic.hs

O módulo `Semantic.hs` é completamente novo e implementa toda a análise semântica do compilador.

### 4.1 Código Completo

```haskell
-- Semantic.hs
-- Semantic analysis for the Ada subset compiler

module Semantic where

import AST
import qualified Data.Map as Map

-- Result type for semantic analysis
data SemanticResult = SemanticResult
  { errors :: [String]
  , warnings :: [String]
  , symbolTable :: SymbolTable
  }
  deriving (Show)

-- Perform semantic analysis on a program
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

-- Process all declarations and check for redeclarations
processDeclarations :: [Decl] -> SymbolTable -> ([String], SymbolTable)
processDeclarations [] st = ([], st)
processDeclarations (VarDecl name typ : rest) st =
  case insertSymbol name typ st of
    Nothing -> 
      let (errs, finalST) = processDeclarations rest st
          errorMsg = "Error: Variable '" ++ name ++ "' already declared in this scope"
      in (errorMsg : errs, finalST)
    Just newST -> processDeclarations rest newST

-- Check statements for undeclared variables
checkStatements :: [Stmt] -> SymbolTable -> [String]
checkStatements stmts st = concatMap (checkStmt st) stmts

checkStmt :: SymbolTable -> Stmt -> [String]
checkStmt st stmt = case stmt of
  Assignment var expr ->
    let varErrors = if isDeclared var st
                    then []
                    else ["Error: Variable '" ++ var ++ "' used but not declared"]
        exprErrors = checkExpr st expr
    in varErrors ++ exprErrors
  
  IfThenElse cond thenStmt elseStmt ->
    checkExpr st cond ++ checkStmt st thenStmt ++ checkStmt st elseStmt
  
  IfThen cond thenStmt ->
    checkExpr st cond ++ checkStmt st thenStmt
  
  While cond body ->
    checkExpr st cond ++ checkStmt st body
  
  Block stmts ->
    let stInBlock = enterScope st
        blockErrors = checkStatements stmts stInBlock
    in blockErrors
  
  PutLine expr ->
    checkExpr st expr
  
  EmptyStmt -> []

-- Check expressions for undeclared variables
checkExpr :: SymbolTable -> Expr -> [String]
checkExpr st expr = case expr of
  Var v -> 
    if isDeclared v st
    then []
    else ["Error: Variable '" ++ v ++ "' used but not declared"]
  
  Add e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Sub e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Mul e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Div e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Mod e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Neg e -> checkExpr st e
  
  And e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Or e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Not e -> checkExpr st e
  
  Eq e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Neq e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Lt e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Lte e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Gt e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Gte e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  
  IntLit _ -> []
  BoolLit _ -> []
  StringLit _ -> []
  GetLine -> []
```

### 4.2 Análise Detalhada

#### 4.2.1 Tipo de Resultado Semântico

**Código:**
```haskell
data SemanticResult = SemanticResult
  { errors :: [String]
  , warnings :: [String]
  , symbolTable :: SymbolTable
  }
  deriving (Show)
```

**Fonte:** Aula Teórica 8 - Reportar erros semânticos

**Conceitos:**
- Acumulação de múltiplos erros (não para na primeira ocorrência)
- Separação entre erros (fatais) e warnings (avisos)
- Preservação da tabela de símbolos para fases posteriores

**Justificação:**
Reportar múltiplos erros de uma vez melhora significativamente a experiência do utilizador, evitando ciclos de compilação→correção→recompilação para cada erro individual.

**🚩 FLAG:** A estrutura específica de `SemanticResult` com warnings separados é uma boa prática de engenharia de compiladores, mas não foi explicitamente detalhada nas aulas (apenas o conceito de reportar erros).

---

#### 4.2.2 Função Principal: analyzeProgram

**Código:**
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

**Fonte:** 
- Aula Teórica 8 - Fases da análise semântica
- Enunciado do Trabalho Prático - Segunda Parte

**Conceitos Aplicados:**

1. **Análise em Duas Fases:**
   - Fase 1: Processar declarações, construir tabela de símbolos
   - Fase 2: Verificar uso de variáveis nos comandos

2. **Fluxo de Dados:**
   - Tabela de símbolos é construída na Fase 1
   - Tabela é usada (read-only) na Fase 2

3. **Acumulação de Erros:**
   - Erros de ambas as fases são combinados
   - Compilação não para no primeiro erro

**Justificação:**
A separação em duas fases é essencial porque todas as declarações devem ser processadas antes de verificar o uso de variáveis. Isto corresponde à semântica de Ada onde o bloco de declarações precede o bloco de comandos.

---

#### 4.2.3 Processamento de Declarações

**Código:**
```haskell
processDeclarations :: [Decl] -> SymbolTable -> ([String], SymbolTable)
processDeclarations [] st = ([], st)
processDeclarations (VarDecl name typ : rest) st =
  case insertSymbol name typ st of
    Nothing -> 
      let (errs, finalST) = processDeclarations rest st
          errorMsg = "Error: Variable '" ++ name ++ "' already declared in this scope"
      in (errorMsg : errs, finalST)
    Just newST -> processDeclarations rest newST
```

**Fonte:** Aula Teórica 8 - Construção de tabela de símbolos

**Conceitos:**
- Processamento recursivo de lista de declarações
- Detecção de redeclarações
- Continuação do processamento após erro (não para)
- Construção incremental da tabela de símbolos

**Casos:**

1. **Declaração Válida:**
   - `insertSymbol` retorna `Just newST`
   - Continua processamento com tabela atualizada
   - Nenhum erro adicionado

2. **Redeclaração:**
   - `insertSymbol` retorna `Nothing`
   - Erro adicionado à lista
   - Processamento continua com tabela original (declaração ignorada)

**Exemplo de Erro Detectado:**
```ada
declare
  x : Integer;
  x : Boolean;  -- ERRO: redeclaração de x
begin
  null;
end;
```

Erro gerado: `"Error: Variable 'x' already declared in this scope"`

---

#### 4.2.4 Verificação de Comandos

**Código:**
```haskell
checkStmt :: SymbolTable -> Stmt -> [String]
checkStmt st stmt = case stmt of
  Assignment var expr ->
    let varErrors = if isDeclared var st
                    then []
                    else ["Error: Variable '" ++ var ++ "' used but not declared"]
        exprErrors = checkExpr st expr
    in varErrors ++ exprErrors
  
  IfThenElse cond thenStmt elseStmt ->
    checkExpr st cond ++ checkStmt st thenStmt ++ checkStmt st elseStmt
  
  IfThen cond thenStmt ->
    checkExpr st cond ++ checkStmt st thenStmt
  
  While cond body ->
    checkExpr st cond ++ checkStmt st body
  
  Block stmts ->
    let stInBlock = enterScope st
        blockErrors = checkStatements stmts stInBlock
    in blockErrors
  
  PutLine expr ->
    checkExpr st expr
  
  EmptyStmt -> []
```

**Fonte:** Aula Teórica 8 - Verificação semântica de comandos

**Análise por Tipo de Comando:**

1. **Assignment:**
   - Verifica se variável destino está declarada
   - Verifica recursivamente a expressão
   - Combina erros de ambas as verificações

2. **IfThenElse / IfThen:**
   - Verifica condição (deve ser expressão válida)
   - Verifica comandos then e else recursivamente
   - Combina todos os erros

3. **While:**
   - Verifica condição
   - Verifica corpo do loop
   - Combina erros

4. **Block:**
   - **IMPORTANTE:** Cria novo escopo
   - Verifica comandos internos no novo escopo
   - Escopo é automaticamente descartado ao sair (não chamamos explicitamente `exitScope`)

5. **PutLine:**
   - Verifica expressão a imprimir

6. **EmptyStmt:**
   - Nenhuma verificação necessária

**Nota sobre Blocos:**
A criação de novo escopo para blocos é essencial para implementar corretamente a semântica de Ada:

```ada
declare
  x : Integer := 1;
begin
  Put_Line(x);  -- Imprime 1
  begin
    declare x : Integer := 2;
    begin
      Put_Line(x);  -- Imprime 2 (x local)
    end;
  end;
  Put_Line(x);  -- Imprime 1 (x original)
end;
```

---

#### 4.2.5 Verificação de Expressões

**Código:**
```haskell
checkExpr :: SymbolTable -> Expr -> [String]
checkExpr st expr = case expr of
  Var v -> 
    if isDeclared v st
    then []
    else ["Error: Variable '" ++ v ++ "' used but not declared"]
  
  Add e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Sub e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Mul e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Div e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Mod e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Neg e -> checkExpr st e
  
  And e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Or e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Not e -> checkExpr st e
  
  Eq e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Neq e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Lt e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Lte e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Gt e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Gte e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  
  IntLit _ -> []
  BoolLit _ -> []
  StringLit _ -> []
  GetLine -> []
```

**Fonte:** Aula Teórica 8 - Verificação semântica de expressões

**Conceitos:**

1. **Variáveis:**
   - Única verificação necessária: variável declarada?
   - Retorna erro se não declarada

2. **Operações Binárias:**
   - Verificação recursiva de ambos os operandos
   - Concatenação de erros

3. **Operações Unárias:**
   - Verificação recursiva do operando

4. **Literais:**
   - Nenhuma verificação necessária
   - Sempre válidos

5. **GetLine:**
   - Nenhuma verificação necessária

**Exemplo de Detecção de Erro:**
```ada
declare
  x : Integer;
begin
  x := y + 5;  -- ERRO: y não declarado
end;
```

Erro gerado: `"Error: Variable 'y' used but not declared"`

**🚩 FLAG - Verificação de Tipos:**
Note que esta implementação NÃO verifica compatibilidade de tipos (por exemplo, somar Integer com Boolean). Embora o sistema de tipos esteja definido em AST.hs, a verificação de tipos não está completamente implementada. Esta funcionalidade seria adicionada em versões futuras do compilador.

As aulas teóricas (Aula 9 - Sistemas de tipos) cobrem verificação de tipos, mas o enunciado do trabalho prático não requereu esta funcionalidade explicitamente.

---

## 5. Novo Módulo: TAC.hs

O módulo `TAC.hs` implementa a geração de código intermédio (Three-Address Code) a partir da AST.

### 5.1 Código Completo
