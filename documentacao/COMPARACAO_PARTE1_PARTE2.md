# 📊 Comparação: Parte 1 vs Parte 2 do Compilador Ada

**Data:** 11 de Dezembro de 2025

---

## 🎯 RESPOSTA RÁPIDA

### Código Manual Escrito para Parte 2:
- **~892 linhas** de código Haskell manual
- **31 novas funções**
- **9 novos tipos de dados**
- **7 blocos funcionais**

### Crescimento Total:
- **+669 linhas de código manual** (+300% vs Parte 1)
- **+3 ficheiros novos** (Semantic.hs, TAC.hs, MIPS.hs)

---

## 📈 COMPARAÇÃO LINHA POR LINHA

### Total de Linhas (incluindo código gerado)
```
Parte 1: 17,348 linhas
Parte 2: 20,467 linhas
Diff:    +3,119 linhas (+18%)
```

### Código Manual (excluindo Lexer/Parser gerados)
```
Parte 1:  ~223 linhas manuais
Parte 2:  ~892 linhas manuais
Diff:     +669 linhas (+300%)
```

---

## 📁 FICHEIROS NOVOS (100% manuais)

| Ficheiro | Linhas | Funções | Descrição |
|----------|--------|---------|-----------|
| **Semantic.hs** | 129 | 5 | Análise semântica, tabela símbolos |
| **TAC.hs** | 248 | 8 | Geração código intermédio |
| **MIPS.hs** | 341 | 12+ | Geração código assembly |
| **TOTAL** | **718** | **25** | **3 módulos novos** |

---

## 📝 FICHEIROS MODIFICADOS

### AST.hs
```
Parte 1:     62 linhas
Parte 2:    183 linhas
Adicionado: +121 linhas (+195%)
```

**Novidades:**
- ✅ 9 novos tipos de dados (Type, Decl, SymbolTable, TAC, etc.)
- ✅ 6 funções para tabela de símbolos
- ✅ Tipo TAC com 6 construtores

### Main.hs
```
Parte 1:    161 linhas
Parte 2:    214 linhas
Adicionado: +53 linhas (+33%)
```

**Novidades:**
- ✅ Import de 3 módulos novos
- ✅ Pipeline completo: AST → Semântica → TAC → MIPS
- ✅ Output para 3 ficheiros (.ast, .tac, .asm)

### Parser.y
```
Parte 1:   1,016 linhas
Parte 2:   1,081 linhas
Adicionado: +65 linhas (+6%)
```

**Novidades:**
- ✅ Regras para declarações de variáveis
- ✅ Produção Program com [Decl]

---

## 🔢 NOVOS TIPOS DE DADOS (9 tipos)

| Tipo | Ficheiro | Construtores/Campos | Propósito |
|------|----------|---------------------|-----------|
| Type | AST.hs | 2 | IntegerType, BooleanType |
| Decl | AST.hs | 1 | VarDecl String Type |
| SymbolInfo | AST.hs | 3 campos | Nome, tipo, nível |
| SymbolTable | AST.hs | 2 campos | Lista scopes, nível |
| Program | AST.hs | 2 campos | [Decl] [Stmt] |
| TAC | AST.hs | 6 | Assign, BinOp, UnOp, Goto, Ifz, Label |
| TACState | TAC.hs | 2 campos | Contadores temp/label |
| MIPSState | MIPS.hs | 4 campos | varMap, offsets, strings |
| SemanticResult | Semantic.hs | 3 campos | errors, warnings, symbolTable |

---

## 🛠️ NOVAS FUNÇÕES (31 funções)

### Tabela de Símbolos (AST.hs) - 6 funções
```haskell
emptySymbolTable :: SymbolTable
enterScope       :: SymbolTable -> SymbolTable
exitScope        :: SymbolTable -> SymbolTable
insertSymbol     :: String -> Type -> SymbolTable -> Maybe SymbolTable
lookupSymbol     :: String -> SymbolTable -> Maybe SymbolInfo
isDeclared       :: String -> SymbolTable -> Bool
```

### Análise Semântica (Semantic.hs) - 5 funções
```haskell
analyzeProgram      :: Program -> SemanticResult
processDeclarations :: [Decl] -> SymbolTable -> ([String], SymbolTable)
checkStatements     :: [Stmt] -> SymbolTable -> [String]
checkStmt           :: SymbolTable -> Stmt -> [String]
checkExpr           :: SymbolTable -> Expr -> [String]
```

### Geração TAC (TAC.hs) - 8 funções
```haskell
generateTAC      :: Program -> [TAC]
generateStmtList :: [Stmt] -> TACState -> ([TAC], TACState)
generateStmt     :: Stmt -> TACState -> ([TAC], TACState)
generateExpr     :: Expr -> TACState -> ([TAC], String, TACState)
generateBinOp    :: Expr -> Expr -> String -> TACState -> ([TAC], String, TACState)
generateUnOp     :: Expr -> String -> TACState -> ([TAC], String, TACState)
newTemp          :: TACState -> (String, TACState)
newLabel         :: TACState -> (String, TACState)
```

### Geração MIPS (MIPS.hs) - 12+ funções
```haskell
generateMIPS       :: [TAC] -> String
generateMIPSText   :: [TAC] -> MIPSState -> String
generateMIPSInstr  :: TAC -> MIPSState -> String
allocateVars       :: [TAC] -> MIPSState -> MIPSState
extractStrings     :: [TAC] -> MIPSState -> (String, MIPSState)
getRegister        :: String -> MIPSState -> String
getRegOrImm        :: String -> MIPSState -> String
isImmediate        :: String -> Bool
isStringLiteral    :: String -> Bool
getStringLabel     :: String -> MIPSState -> String
mipsPreamble       :: String
mipsPostamble      :: String
```

---

## 🧩 7 BLOCOS FUNCIONAIS PRINCIPAIS

### Bloco 1: Sistema de Tipos (AST.hs)
**~40 linhas**
```haskell
data Type = IntegerType | BooleanType
data Decl = VarDecl String Type
data Program = Program [Decl] [Stmt]  -- modificado
```

### Bloco 2: Tabela de Símbolos (AST.hs)
**~60 linhas**
```haskell
data SymbolTable = ...
emptySymbolTable, enterScope, exitScope
insertSymbol, lookupSymbol, isDeclared
```

### Bloco 3: Tipo TAC (AST.hs)
**~20 linhas**
```haskell
data TAC = Assign | BinOp | UnOp | Goto | Ifz | Label
```

### Bloco 4: Análise Semântica (Semantic.hs)
**~100 linhas**
- Verificação de declarações
- Verificação de uso de variáveis
- Gestão de escopos aninhados
- Detecção de erros

### Bloco 5: Geração TAC (TAC.hs)
**~200 linhas**
- Tradução AST → TAC
- Geração de temporários
- Geração de labels
- Estado funcional puro

### Bloco 6: Geração MIPS (MIPS.hs)
**~300 linhas**
- Alocação de registos
- Tradução TAC → MIPS
- Seção .data (strings)
- Seção .text (código)
- Constant folding (otimização)

### Bloco 7: Integração (Main.hs)
**~50 linhas**
```haskell
parse → analyze → generateTAC → generateMIPS
        ↓          ↓              ↓
     errors?    .tac file    .asm file
```

---

## 📊 DISTRIBUIÇÃO DE CÓDIGO MANUAL

### Por Fase do Compilador:

```
┌────────────────────────────────────────────┐
│ Parte 1 (Análise Léxica + Sintática)      │
├────────────────────────────────────────────┤
│ AST.hs:    62 linhas                       │
│ Main.hs:  161 linhas                       │
│ TOTAL:    223 linhas manuais               │
└────────────────────────────────────────────┘

┌────────────────────────────────────────────┐
│ Parte 2 (Semântica + TAC + MIPS)          │
├────────────────────────────────────────────┤
│ Semantic.hs: 129 linhas (novo)             │
│ TAC.hs:      248 linhas (novo)             │
│ MIPS.hs:     341 linhas (novo)             │
│ AST.hs:     +121 linhas (extensão)         │
│ Main.hs:     +53 linhas (extensão)         │
│ TOTAL:       892 linhas manuais            │
└────────────────────────────────────────────┘

CRESCIMENTO: +669 linhas (+300%)
```

### Por Tipo de Código:

| Categoria | Linhas | % do Total |
|-----------|--------|------------|
| **Análise Semântica** | 129 + 60 = 189 | 21% |
| **Geração TAC** | 248 + 20 = 268 | 30% |
| **Geração MIPS** | 341 | 38% |
| **Integração** | 53 | 6% |
| **Tipos/Infra** | 41 | 5% |

---

## 💡 INSIGHTS

### Complexidade da Parte 2:
1. **Análise Semântica** (189 linhas, 11 funções)
   - Mais simples: verificação de regras
   - Recursão sobre AST
   
2. **Geração TAC** (268 linhas, 8 funções)
   - Complexidade média: tradução 1-para-N
   - Gestão de estado funcional

3. **Geração MIPS** (341 linhas, 12+ funções)
   - Mais complexo: alocação registos, otimizações
   - Tradução com constantes e pseudo-instruções

### Razão código manual/gerado:
- Parte 1: 223 manual / 16,000 gerado = **1.4%**
- Parte 2: 892 manual / 18,000 gerado = **4.7%**

**Parte 2 tem 3x mais código manual que Parte 1!**

---

## 🎯 CONCLUSÃO

### Trabalho Necessário para Parte 2:

**Escrito do zero:**
- ✅ 718 linhas em 3 módulos novos
- ✅ 25 novas funções principais
- ✅ 6 novos tipos de dados

**Modificações em código existente:**
- ✅ +174 linhas em AST.hs e Main.hs
- ✅ +65 linhas em Parser.y

**TOTAL: ~892 linhas de código Haskell manual**

**Esforço estimado:**
- Parte 1: ~223 linhas manual (Lexer/Parser usam geradores)
- Parte 2: ~892 linhas manual (tudo escrito à mão)
- **Parte 2 é 4x maior em código manual!**

---

## 📈 EVOLUÇÃO DO PROJETO

```
Parte 1                    Parte 2
════════                   ═══════════
                          
Ada source                 Ada source
    ↓                         ↓
  Lexer ←──────────→      Lexer
    ↓                         ↓
  Parser ←─────────→       Parser
    ↓                         ↓
   AST                       AST
    ↓                         ↓
   (fim)                  Semantic ← NOVO
                              ↓
                            TAC    ← NOVO
                              ↓
                            MIPS   ← NOVO
                              ↓
                          Assembly

223 linhas               892 linhas
+0%                      +300%
```

---

**Fim da Comparação**
