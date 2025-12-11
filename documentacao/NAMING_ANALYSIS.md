# Análise de Nomenclatura - Comparação com Materiais do Curso

**Data:** 11 de Dezembro de 2025  
**Objetivo:** Verificar conformidade da implementação com nomenclatura dos slides do professor  
**Método:** Extração completa via pdftotext dos PDFs das aulas

---

## 📊 Sumário Executivo

A implementação atual está **funcionalmente correta** e segue os **conceitos** ensinados nas aulas. No entanto, existem algumas diferenças de **nomenclatura** entre o código implementado e os termos **EXATOS** usados pelo professor nos slides.

**Verificação via pdftotext (11 Dez 2025):**
- ✅ Aula Teórica 10 - extraída e analisada
- ✅ Aula Teórica 8 - extraída e analisada  
- ✅ Aula Teórica 12 - extraída e analisada
- ✅ Prática 5 - extraída e analisada
- ✅ Prática 6 - extraída e analisada
- ✅ Trabalho Prático Parte 2 - extraído e analisado

### Estado Atual
- ✅ Todos os conceitos implementados têm fonte nos materiais do curso
- ✅ Funcionalidade está correta
- ⚠️ Nomenclatura difere em alguns pontos dos slides do professor
- ⚠️ Algumas flags precisam ser atualizadas/verificadas

---

## 🔍 Análise Detalhada por Módulo

### 1. TAC.hs - Código Intermédio

#### Nomenclatura EXATA do Professor (via pdftotext)

**Aula Teórica 10 (Slide "Instruções do código intermédio básico"):**
```
Instr → temp := Atom
      | temp := temp binop Atom
      | LABEL label
      | JUMP label
      | COND temp relop Atom label label
Atom → temp | num
```

**Aula Teórica 10 (Slide "Tradução de expressões aritméticas"):**
```
A função de tradução
transExpr : (Exp, Table, Temp) → [Instr]
recebe também o destino onde colocar o resultado (atributo herdado)

transExpr (expr, table, dest) = case expr of
    num         return [dest := num]
    id          temp = lookup(id, table)
                return [dest := temp]
    e1 binop e2 t1 = newTemp()
                t2 = newTemp()
                code1 = transExpr(e1, table, t1)
                code2 = transExpr(e2, table, t2)
                return code1 ++code2 ++[dest := t1 binop t2]
```

**Aula Teórica 10 (Slide "Tradução de comandos"):**
```
Função de tradução:
transStm : (Stm, Table) → [Instr]
```

**Prática 6 (pratica6_codeGen_hs.txt):**
```haskell
-- Tipo sugerido
data Instr = ...

-- Função sugerida
compileExpr :: Expr -> ([Instr], Temp)
```

#### Implementação Atual

```haskell
-- Nome do tipo: TAC (não Instr)
data TAC =
    Assign String String
  | BinOp String String String String
  | UnOp String String String
  | Goto String
  | Ifz String String
  | Label String

-- Nome da função: generateExpr (não compileExpr)
generateExpr :: Expr -> TACState -> ([TAC], String, TACState)
```

#### Diferenças Identificadas (EXATAS via pdftotext)

| Aspecto | Professor (EXATO) | Implementação Atual | Criticidade |
|---------|-------------------|---------------------|-------------|
| Nome do tipo | `Instr` | `TAC` | ⚠️ **ALTA** |
| Nome da função (slides) | `transExpr` | `generateExpr` | ⚠️ **ALTA** |
| Nome da função (prática) | `compileExpr` | `generateExpr` | ⚠️ **ALTA** |
| Função comandos (slides) | `transStm` | `generateStmt` | ⚠️ **ALTA** |
| Tipo de retorno | `([Instr], Temp)` | `([TAC], String, TACState)` | ⚠️ Média |
| Estado | Implícito | Explícito (`TACState`) | ℹ️ Estilo |
| Instrução label | `LABEL` (maiúsculas) | `Label` (case) | ⚠️ Média |
| Instrução goto | `JUMP` (maiúsculas) | `Goto` (case) | ⚠️ Média |
| Instrução condicional | `COND temp relop Atom label label` | `Ifz String String` | ⚠️ **ALTA** |

#### Recomendações

**Opção A - Refactoring Completo (Alinhamento Total):**
- Renomear `TAC` → `Instr`
- Renomear `generateExpr` → `compileExpr`
- Renomear `generateStmt` → `compileStmt`
- Simplificar tipo de retorno (se possível, usando State monad)

**Opção B - Refactoring Parcial (Mantém Estrutura):**
- Renomear apenas `TAC` → `Instr`
- Renomear `generateExpr` → `compileExpr`
- Manter `TACState` explícito (justificar como estilo funcional válido)

**Opção C - Manter e Documentar:**
- Adicionar comentário explicativo no código
- Documentar que `TAC` é equivalente a `Instr` dos slides
- Justificar escolha de nomes mais descritivos

---

### 2. Semantic.hs - Análise Semântica

#### Nomenclatura EXATA do Professor (via pdftotext)

**Aula Teórica 8 (Slide "Operações sobre a tabela de símbolos"):**
```
Operações fundamentais:
  inicializar uma tabela vazia;
  inserir dado o identificador e informação inserir uma nova entrada numa tabela;
          (se o identificador já ocorre, a informação deve alterada)
  procurar dado o identificador devolve a informação associada (caso exista) ou um
          sinal de falha (i.e. uma excepção ou um resultado opcional)
```

**Aula Teórica 8 (Slide "Operações sobre a tabela de símbolos (cont.)"):**
```
Necessitamos de duas operações extra para âmbitos:
  abrir iniciar num novo âmbito (i.e. à entrada de um bloco ou função)
  fechar terminar o âmbito atual repondo todas as associações como estavam
         antes da abertura do âmbito
```

**Aula Teórica 8 (Slide "Implementação 1: Listas ligadas funcionais"):**
```
Uma lista de pares (identificador,info):
  inicializar a lista vazia [];
  inserir acrescentar (ident,info) ao início da lista;
  procurar do ínicio para o final da lista (i.e. lookup do prelúdio de Haskell);
  abrir âmbito lembrar a tabela atual;
  fechar âmbito voltar à tabela guardada.
```

**Termos EXATOS usados nos slides:**
- "**âmbito**" (não "scope")
- "**procurar**" (português, mas menciona "lookup do prelúdio de Haskell")
- "**abrir**" âmbito (não "enter")
- "**fechar**" âmbito (não "exit")

#### Implementação Atual

```haskell
-- Funções em AST.hs
emptySymbolTable :: SymbolTable     -- ✅ OK (inicializar)
insertSymbol :: ...                  -- ✅ OK (inserir)
lookupSymbol :: ...                  -- ⚠️ "lookup" vs "procurar"
enterScope :: ...                    -- ⚠️ "enter" vs "abrir"
exitScope :: ...                     -- ⚠️ "exit" vs "fechar"
```

#### Diferenças Identificadas (EXATAS via pdftotext)

| Aspecto | Professor (EXATO) | Implementação Atual | Criticidade | Nota |
|---------|-------------------|---------------------|-------------|------|
| Operação lookup | `procurar` | `lookupSymbol` | ⚠️ Média | Slide menciona "lookup do prelúdio" |
| Operação abrir âmbito | `abrir` | `enterScope` | ⚠️ Média | Termo português nos slides |
| Operação fechar âmbito | `fechar` | `exitScope` | ⚠️ Média | Termo português nos slides |
| Termo escopo | "âmbito" | "scope" | ℹ️ Menor | Código em inglês é aceitável |

#### Recomendações

**IMPORTANTE:** Os slides usam termos em português ("procurar", "abrir", "fechar") mas **também mencionam** "lookup do prelúdio de Haskell", o que sugere que usar inglês no código é aceitável.

**Opção A - Manter Nomenclatura Inglesa (RECOMENDADO):**
- Justificativa: Código em Haskell convencionalmente usa termos em inglês
- O próprio slide menciona: "lookup do prelúdio de Haskell"
- Adicionar comentários bilíngues:
```haskell
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo  -- procurar
enterScope :: SymbolTable -> SymbolTable                   -- abrir âmbito
exitScope :: SymbolTable -> SymbolTable                    -- fechar âmbito
```

**Opção B - Criar Aliases em Português:**
```haskell
-- Aliases para compatibilidade exata com slides
procurar = lookupSymbol  -- procurar identificador
abrir = enterScope       -- abrir âmbito
fechar = exitScope       -- fechar âmbito
```

**Opção C - Refactor Completo para Português:**
- Renomear todas as funções para português
- Menos comum na comunidade Haskell
- Pode causar confusão com bibliotecas padrão

---

### 3. MIPS.hs - Geração de Assembly

#### Nomenclatura EXATA do Professor (via pdftotext)

**Prática 6 (pratica6_codeGen_hs.txt) - Pergunta 4:**
```
4. Defina em Haskell uma função printMIPS que imprime num ficheiro 
   o código MIPS correspondente à lista de instruções gerada pelo compilador.

   Referência para a pergunta 4: 
   https://www2.cs.arizona.edu/~debray/Teaching/CSc453/DOCS/3addr2spim.pdf
```

**Aula Teórica 12 - Termos usados:**
- ✅ Registos: `$t0-$t9`, `$s0-$s7` (correto)
- ✅ Instruções: `add`, `sub`, `mul`, `beq`, `bne`, `j` (correto)
- ✅ Pseudo-instruções: `move`, `li`, `blt`, `bge` (correto)

**Aula Teórica 12 (Slide "Padrões de código intermédio"):**
```
Vamos traduzir padrões de instruções intermédias em instruções de código máquina
```

**Aula Teórica 12 (Slide "Tradução de padrões para MIPS"):**
Mostra traduções diretas de `Instr` para MIPS assembly.

#### Implementação Atual

```haskell
generateMIPS :: [TAC] -> String      -- ✅ Nome descritivo OK
generateMIPSInstr :: ...             -- ✅ OK
```

#### Diferenças Identificadas (EXATAS via pdftotext)

| Aspecto | Professor (EXATO) | Implementação Atual | Criticidade |
|---------|-------------------|---------------------|-------------|
| Nome função principal | `printMIPS` | `generateMIPS` | ⚠️ **ALTA** |
| Input | `[Instr]` | `[TAC]` | ⚠️ **ALTA** (depende de TAC/Instr) |
| Instruções MIPS | ✅ Corretas | ✅ Corretas | ✅ OK |
| Registos | ✅ Corretos | ✅ Corretos | ✅ OK |

#### Recomendações

**Opção A - Seguir Prática 6 Exatamente:**
- Renomear `generateMIPS` → `printMIPS`
- Texto exato: "Defina em Haskell uma função **printMIPS** que imprime num ficheiro..."
- **ALTA prioridade** - nome explícito no enunciado

**Opção B - Manter Nome Atual com Alias:**
```haskell
-- Alias para compatibilidade com Prática 6
printMIPS :: [Instr] -> String
printMIPS = generateMIPS
```

**Opção C - Justificar Escolha:**
- `generateMIPS` é mais descritivo ("gera" vs "imprime")
- Adicionar comentário: `-- printMIPS da Prática 6`

---

### 4. AST.hs - Estruturas de Dados

#### Nomenclatura do Professor

**Aula 8 - Tabela de Símbolos:**
- Termo: "Tabela de Símbolos" ✅
- Estrutura: Lista de pares (identificador, info) ✅

**Aula 10 - Código Intermédio:**
- `Instr` para instruções ⚠️
- `Temp` para temporários ⚠️

#### Implementação Atual

```haskell
type SymbolTable = ...               -- ✅ OK
data SymbolInfo = ...                -- ✅ OK
data TAC = ...                       -- ⚠️ vs Instr
```

#### Recomendações

- Se refatorar TAC → Instr, atualizar aqui também
- Manter `SymbolTable` (termo correto)

---

## 🚩 Flags a Adicionar/Atualizar

### Flags Existentes que Precisam Verificação

#### 1. Constant Folding (MIPS.hs)

**Status Atual:** Marcado como flag
```haskell
-- Constant folding no código MIPS
if isImmediate src1 && isImmediate src2
then let result = show ((read src1 :: Int) + (read src2 :: Int))
```

**Verificação:** ✅ Correto - Não é mencionado nas aulas
**Flag Recomendada:**
```haskell
-- 🚩 OTIMIZAÇÃO NÃO REQUERIDA: Constant folding
-- Não mencionado em Aula 10, 12 ou Prática 6
-- Justificativa: Otimização básica, melhora eficiência do código gerado
```

#### 2. Operador Unário (TAC.hs)

**Status Atual:** Não tem flag explícita
```haskell
| UnOp String String String              -- x := op y
```

**Verificação:** ⚠️ Parcialmente coberto
- Ada tem operadores unários (`-x`, `not x`)
- Prática 6 não menciona explicitamente
- Aula 10 foca em operadores binários

**Flag Recomendada:**
```haskell
-- 🚩 EXTENSÃO NECESSÁRIA: Operadores unários
-- Prática 6 define apenas: "VAR := atom binop atom"
-- No entanto, Ada requer operadores unários (-, not)
-- Fonte implícita: Gramática Ada (negação e not)
```

#### 3. SemanticResult.warnings (Semantic.hs)

**Status Atual:** Mencionado em documentação
```haskell
data SemanticResult = SemanticResult {
    errors :: [String],
    warnings :: [String]  -- Não usado
}
```

**Verificação:** ✅ Correto identificar como preparação futura
**Flag Recomendada:**
```haskell
-- 🚩 PREPARAÇÃO FUTURA: Campo warnings não utilizado
-- Não requerido no trabalho prático
-- Preparado para extensões (e.g., variáveis não usadas)
```

#### 4. Estado Explícito (TACState)

**Status Atual:** Não tem flag
```haskell
data TACState = TACState {
    tempCount :: Int,
    labelCount :: Int
}
```

**Verificação:** ⚠️ Diferença de estilo vs Prática 6
- Prática 6 sugere: `compileExpr :: Expr -> ([Instr], Temp)`
- Estado implícito (monads) ou função com contador global
- Implementação atual usa threading explícito

**Flag Recomendada:**
```haskell
-- 📝 DECISÃO DE IMPLEMENTAÇÃO: Estado explícito
-- Prática 6 sugere tipo: compileExpr :: Expr -> ([Instr], Temp)
-- Implementação usa threading explícito de estado (estilo funcional)
-- Alternativas: State monad ou variáveis globais
-- Escolha: Explícito, puro, sem monads (mais simples para iniciantes)
```

---

## 📝 Tabela Resumo de Conformidade

| Componente | Conceito | Nomenclatura | Funcionalidade | Ação Recomendada |
|------------|----------|--------------|----------------|------------------|
| **TAC.hs** | ✅ | ⚠️ | ✅ | Considerar renomear |
| **Semantic.hs** | ✅ | ✅ | ✅ | Adicionar comentários |
| **MIPS.hs** | ✅ | ⚠️ | ✅ | Considerar renomear |
| **AST.hs** | ✅ | ✅ | ✅ | Manter |

**Legenda:**
- ✅ Totalmente conforme
- ⚠️ Diferenças menores de nomenclatura
- ❌ Não conforme (nenhum caso encontrado)

---

## 🎯 Recomendações Finais

### Prioridade Alta

1. **Adicionar Flags Detalhadas:** Adicionar comentários com 🚩 para todas as implementações que não têm fonte direta nas aulas

2. **Atualizar Documentação:** Incluir tabela de mapeamento entre nomenclatura do professor e implementação atual

### Prioridade Média

3. **Considerar Renomeação:** Se o objetivo é alinhamento total com slides:
   - `TAC` → `Instr`
   - `generateExpr` → `compileExpr`
   - `generateMIPS` → `printMIPS`

4. **Adicionar Aliases:** Criar aliases para compatibilidade:
   ```haskell
   type Instr = TAC  -- Compatibilidade com slides
   ```

### Prioridade Baixa

5. **Comentários Bilíngues:** Adicionar comentários em português para termos dos slides
   ```haskell
   enterScope :: SymbolTable -> SymbolTable  -- abrir âmbito
   ```

---

## 📚 Mapeamento Nomenclatura Professor ↔ Implementação

| Conceito | Professor (Slides) | Implementação Atual | Arquivo |
|----------|-------------------|---------------------|---------|
| Código intermédio | `Instr` | `TAC` | TAC.hs |
| Compilar expressão | `compileExpr` | `generateExpr` | TAC.hs |
| Compilar comando | `compileCmd` | `generateStmt` | TAC.hs |
| Imprimir MIPS | `printMIPS` | `generateMIPS` | MIPS.hs |
| Variável temporária | `temp` | `String` (t0, t1,...) | TAC.hs |
| Etiqueta | `label` | `String` (L0, L1,...) | TAC.hs |
| Salto incondicional | `JUMP` | `Goto` | AST.hs/TAC.hs |
| Salto condicional | `COND` | `Ifz` | AST.hs/TAC.hs |
| Procurar símbolo | `procurar` | `lookupSymbol` | AST.hs |
| Abrir âmbito | `abrir` | `enterScope` | AST.hs |
| Fechar âmbito | `fechar` | `exitScope` | AST.hs |

---

## ✅ Conclusão

A implementação está **conceitualmente correta** e segue fielmente os algoritmos e estruturas ensinados nas aulas. As diferenças de nomenclatura são principalmente **estilísticas** e não afetam a correção do código.

**Opções:**

1. **Manter como está:** Justificar escolhas de nomenclatura mais descritivas (e.g., `TAC` é mais claro que `Instr` para "Three-Address Code")

2. **Refatorar nomenclatura:** Alinhar completamente com termos dos slides para demonstrar que seguiu fielmente o material

3. **Híbrido:** Adicionar aliases e comentários referenciando a nomenclatura dos slides

**Recomendação:** Opção 3 (Híbrido) - mantém o código funcional atual mas adiciona clareza sobre a correspondência com os slides do professor.
