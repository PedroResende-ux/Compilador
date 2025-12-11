# Fontes Detalhadas - Segunda Parte do Compilador Ada

Este documento mapeia cada componente da implementação às suas fontes específicas nos materiais do curso.

---

## 📚 Materiais do Curso Utilizados

### Trabalhos Práticos
1. **Trabalho Prático - primeira parte (enunciado).pdf**
   - Put_Line e Get_Line explicitamente requeridos
   - "The subset of Ada considered... the Put_Line function (output); the Get_Line function (input)"

2. **Trabalho Prático - segunda parte (enunciado).pdf**
   - Requisitos da segunda parte:
     - "Build a symbol table with type and any other semantic information"
     - "Implement an intermediate code generator... three address code"
     - "Implement a code generator for MIPS"

### Aulas Teóricas
1. **Aula teórica 8 - Análise semântica e tabelas de símbolos.pdf**
2. **Aula teórica 9 - Sistemas de tipos.pdf**
3. **Aula teórica 10 - Geração de código intermédio.pdf**
4. **Aula teórica 12 - Emissão de código assembler.pdf**

### Aulas Práticas
1. **pratica5_3address.pdf** - Exercícios de tradução para 3-endereços
2. **pratica6_codeGen_hs.txt** - Template em Haskell para geração de código

### Bibliografia
1. **Saumya Debray. Notes on Translating 3-address Code to MIPS Assembly Code.pdf**

---

## 🗂️ Mapeamento Detalhado por Módulo

### 1. Semantic.hs - Análise Semântica

#### **Fonte Principal:** Aula teórica 8

**Seção: "Operações sobre a tabela de símbolos"**
```
Operações fundamentais:
  • inicializar uma tabela vazia;
  • inserir dado o identificador e informação
  • lookup - procurar identificador na tabela
```

**Código implementado:**
```haskell
data SemanticResult = SemanticResult {
    symbolTable :: SymbolTable,
    errors :: [String],
    warnings :: [String]
} deriving (Show)

checkProgram :: Program -> SemanticResult
```

**Operações de tabela de símbolos:**
- `insertSymbol` → "inserir uma nova entrada numa tabela"
- `lookupSymbol` → "procurar identificador na tabela"
- `enterScope` → "abrir - iniciar num novo âmbito"
- `exitScope` → "fechar - sair de um âmbito"

**Localização no PDF:**
- Linha 202: "Operações sobre a tabela de símbolos"
- Linha 216: "Operações sobre a tabela de símbolos (cont.)"

---

### 2. TAC.hs - Código Intermédio de Três Endereços

#### **Fonte Principal:** Aula teórica 10

**Seção: "Código de três endereços"** (linha 77-80)
```
Vamos estudar um código intermédio de três endereços:
  ▶ Número arbitrário de registos temporários
  ▶ Operações com 2 ou 3 operandos
  ▶ Sem instruções de processadores específicos
```

**Estrutura TAC implementada:**
```haskell
data TAC =
    Assign String String                   -- x := y
  | BinOp String String String String      -- x := y op z
  | UnOp String String String              -- x := op y
  | Goto String                            -- goto label
  | Ifz String String                      -- ifz x goto label
  | Label String                           -- label:
```

**Exemplo da Aula 10:**
```
x = 3*(4+5)
Decompor em:
t1 = 3
t2 = 4
t3 = 5
t4 = t2+t3
t5 = t1*t4
x = t5
```

**Geração de temporários** (linha 168):
```
"Para gerar nomes temporários usamos pseudo-funções"
```

**Implementação:**
```haskell
newTemp :: TACState -> (String, TACState)
newTemp state = ("t" ++ show (tempCount state), 
                 state { tempCount = tempCount state + 1 })
```

---

#### **Fonte Secundária:** Prática 5 (pratica5_3address.pdf)

**Exercícios de tradução:**
```
Exercício 1: Traduza as seguintes instruções para código de 3 endereços:
(a) y = 1+x+3*x*x;
(c) if(x<0) x = -1*x;
(d) y = (x == 1) || (x == 2);
```

Estes exercícios mostram o formato esperado de TAC.

---

#### **Fonte Terciária:** Prática 6 (pratica6_codeGen_hs.txt)

**Template de função sugerido:**
```
compileExpr :: Expr -> ([Instr], Temp)
```

**Nossa implementação equivalente:**
```haskell
generateExpr :: Expr -> TACState -> ([TAC], String, TACState)
```

**Diferença:** Adicionamos threading de estado explícito ao invés de usar monads.

---

### 3. MIPS.hs - Geração de Código Assembly

#### **Fonte Principal:** Aula teórica 12

**Seção: "Arquitetura MIPS"** (linha 60-66)
```
  ▶ 32 registos inteiros $0 – $31 de 32-bits
  ▶ Operações entre 3 registos ou registos e constantes (immediate)
```

**Seção: "Tradução de padrões para MIPS"** (linhas 266, 318)

Exemplos de traduções TAC → MIPS apresentados nos slides.

**Comparações em MIPS** (linha 192):
```
"Em MIPS comparações = e ≠ são diretas (beq e bne) mas <, > etc. 
devem usar pseudo-instruções slt, sle, etc."
```

**Implementação:**
```haskell
tacToMIPS (BinOp dest src1 src2 "Lt") state =
    let src1Reg = getRegOrImm src1 state
        src2Reg = getRegOrImm src2 state
        destReg = allocateReg dest state
    in ("  slt " ++ destReg ++ ", " ++ src1Reg ++ ", " ++ src2Reg, state)
```

---

#### **Fonte Secundária:** Debray Paper

**Título completo:** "Notes on Translating 3-address Code to MIPS Assembly Code"

**Referência explícita no código** (MIPS.hs, linha 4):
```haskell
-- Reference: Saumya Debray, "Notes on Translating 3-address Code to MIPS"
```

**Seções utilizadas:**

1. **Register Allocation** - Estratégia de alocação de registos
   - $s0-$s7 para variáveis persistentes
   - $t0-$t9 para temporários
   - $a0-$a3 para argumentos/syscalls

2. **Stack Management** - Gestão de pilha (preparado mas não implementado)

3. **Instruction Translation Patterns** - Padrões de tradução de instruções

**Nota:** O paper Debray está referenciado explicitamente na Prática 6:
```
"Referência para a pergunta 4: 
https://www2.cs.arizona.edu/~debray/Teaching/CSc453/DOCS/3addr2spim.pdf"
```

---

### 4. AST.hs - Extensões para Segunda Parte

#### **Tipos de Dados**

**Fonte:** Aula teórica 9 - "Sistemas de tipos"

```haskell
data Type = IntegerType | BooleanType
  deriving (Show, Eq)
```

---

#### **Tabela de Símbolos**

**Fonte:** Aula teórica 8 (linha 64-65)
```
"(variáveis, funções, etc.) numa tabela de símbolos
A tabela de símbolos será também usada para geração de código"
```

**Estrutura implementada:**
```haskell
type SymbolTable = [[Scope]]
type Scope = [(String, SymInfo)]

data SymInfo = SymInfo {
    symType :: Type,
    symInit :: Bool
} deriving (Show, Eq)
```

---

#### **Operações de Escopo**

**Fonte:** Aula teórica 8 (linha 216+)
```
"Necessitamos de duas operações extra para âmbitos:
  • abrir - iniciar num novo âmbito
  • fechar - sair de um âmbito"
```

**Implementação:**
```haskell
enterScope :: SymbolTable -> SymbolTable
enterScope st = [] : st

exitScope :: SymbolTable -> SymbolTable
exitScope (_:rest) = rest
exitScope [] = error "Cannot exit global scope"
```

---

### 5. Main.hs - Pipeline do Compilador

#### **Fonte:** Aula teórica 10 (início)

**Diagrama de fases do compilador:**
```
texto do programa
  ↓ Análise lexical
sequência de tokens
  ↓ Análise sintática
árvore sintática abstrata
  ↓ Análise semântica
AST & tabela de símbolos
  ↓ Geração de código
código intermédio
  ↓ Seleção de instruções
código assembly simbólico
```

**Implementação do pipeline:**
```haskell
main = do
    -- Fase 1: Análise Léxica
    let tokens = alexScanTokens input
    
    -- Fase 2: Análise Sintática
    let ast = parser tokens
    
    -- Fase 3: Análise Semântica
    let semResult = checkProgram ast
    
    -- Fase 4: Geração de TAC
    let tacCode = generateTAC ast
    
    -- Fase 5: Geração de MIPS
    let (mipsCode, _) = generateMIPS tacCode initMIPSState
```

---

## 🚩 Implementações sem Fonte Direta

### 1. Constant Folding (MIPS.hs)

**Código:**
```haskell
"Add" -> 
    if isImmediate src1 && isImmediate src2
    then let result = show ((read src1 :: Int) + (read src2 :: Int))
         in "  li " ++ destReg ++ ", " ++ result
```

**Status:** ⚠️ Otimização não mencionada nas aulas
**Justificativa:** Otimização básica, boa prática de compiladores

---

### 2. UnOp para Operadores Unários

**Código:**
```haskell
| UnOp String String String    -- x := op y
```

**Status:** ⚠️ Extensão necessária
**Justificativa:** Ada tem operadores unários (`-x`, `not x`) que precisam ser compilados
**Nota:** Prática 6 só menciona binários, mas operadores unários são implícitos em Ada

---

### 3. Estrutura SemanticResult

**Código:**
```haskell
data SemanticResult = SemanticResult {
    symbolTable :: SymbolTable,
    errors :: [String],
    warnings :: [String]  -- ⚠️ Preparado mas não usado
}
```

**Status:** ⚠️ Campo `warnings` não utilizado
**Justificativa:** Preparação para extensões futuras, boa prática de engenharia

---

### 4. Alocação Específica de Registos

**Código:**
```haskell
allocateReg :: String -> MIPSState -> String
allocateReg var state =
    case lookup var (varRegs state) of
        Just reg -> reg
        Nothing -> 
            let regNum = length (varRegs state) `mod` 8
            in "$s" ++ show regNum
```

**Status:** ⚠️ Estratégia específica implementada
**Justificativa:** Aula 12 menciona alocação de registos mas não detalha algoritmo específico
**Referência parcial:** Debray paper discute convenções MIPS

---

## 📊 Tabela Resumo de Fontes

| Componente | Fonte Principal | Fonte Secundária | Status |
|------------|----------------|------------------|--------|
| **Semantic.hs** | Aula 8 (tabelas símbolos) | Trabalho Parte 2 | ✅ Completo |
| **TAC.hs - estrutura** | Aula 10 (3-endereços) | Prática 5 | ✅ Completo |
| **TAC.hs - geração** | Aula 10 (temporários) | Prática 6 template | ✅ Completo |
| **MIPS.hs - instruções** | Aula 12 (padrões MIPS) | Debray paper | ✅ Completo |
| **MIPS.hs - registos** | Aula 12 (arquitetura) | Debray (convenções) | ✅ Completo |
| **AST.hs - tipos** | Aula 9 (sistemas tipos) | Trabalho Parte 1 | ✅ Completo |
| **AST.hs - escopo** | Aula 8 (operações) | - | ✅ Completo |
| **Main.hs - pipeline** | Aula 10 (diagrama) | - | ✅ Completo |
| **I/O (Put_Line/Get_Line)** | Trabalho Parte 1 | - | ✅ Requerido |
| **Constant folding** | - | - | ⚠️ Otimização |
| **UnOp** | Ada (implícito) | - | ⚠️ Extensão necessária |

---

## 📖 Citações Diretas dos Materiais

### Trabalho Prático - Segunda Parte
```
"3. Implement a code generator for MIPS which, given a list of three-address
    instructions, prints on a file its corresponding MIPS code. MIPS code
    should be tested using https://github.com/dpetersanderson/MARS."
```

### Aula 10
```
"Exemplo de motivação
Queremos gerar codigo para a atribuição
x = 3*(4+5)
Podemos decompor em atribuições mais simples usando variáveis temporárias:
t1 = 3
t2 = 4
t3 = 5
t4 = t2+t3
t5 = t1*t4
x = t5"
```

### Prática 6
```
"2. a) Defina um data (Instr) em Haskell para representar um código 
      intermédio de três endereços definido por:
   instruction -> VAR := atom | VAR := atom binop atom
   atom -> VAR | NUMBER
   binop -> PLUS | MINUS | DIV | MULT"
```

---

## ✅ Validação de Conformidade

**Checklist de requisitos do Trabalho Parte 2:**

1. ✅ "Build a symbol table with type and any other semantic information"
   - Implementado em `Semantic.hs` e `AST.hs`
   - Fonte: Aula 8

2. ✅ "Implement an intermediate code generator... three address code"
   - Implementado em `TAC.hs`
   - Fonte: Aula 10, Prática 5, Prática 6

3. ✅ "Implement a code generator for MIPS"
   - Implementado em `MIPS.hs`
   - Fonte: Aula 12, Debray paper

4. ✅ "MIPS code should be tested using MARS"
   - Testado com ficheiros `.asm` gerados
   - Instruções incluem syscalls compatíveis com MARS

---

## 📝 Notas Importantes

1. **Todas as funcionalidades principais têm fontes diretas** nos materiais do curso
2. **I/O (Put_Line/Get_Line) estava explicitamente requerido** desde a Parte 1
3. **Otimizações (constant folding) são claramente marcadas** como extensões
4. **Estrutura geral segue exatamente** o pipeline apresentado na Aula 10
5. **Implementação em Haskell segue** o template da Prática 6

---

**Data de criação:** 11 de Dezembro de 2025
**Versão:** 1.0 com fontes detalhadas verificadas via pdftotext
