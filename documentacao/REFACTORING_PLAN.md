# Plano de Refactoring - Alinhamento com Nomenclatura do Professor

**Data:** 11 de Dezembro de 2025  
**Baseado em:** Extração via pdftotext dos PDFs das aulas

---

## 🎯 Objetivo

Alinhar a nomenclatura do código com os termos **EXATOS** usados pelo professor nos slides, mantendo a funcionalidade intacta.

---

## 📋 Mudanças Prioritárias (Ordenadas por Criticidade)

### ⚠️ PRIORIDADE ALTA - Nomenclatura Explícita nos Materiais

#### 1. Renomear Tipo `TAC` → `Instr`

**Fonte:** Aula Teórica 10, slide "Instruções do código intermédio básico"
```
Instr → temp := Atom
      | temp := temp binop Atom
      | LABEL label
      | JUMP label
      | COND temp relop Atom label label
```

**Arquivos afetados:**
- `AST.hs` - definição do tipo
- `TAC.hs` - todas as referências
- `MIPS.hs` - tipo de input
- `Main.hs` - referências ao tipo

**Mudança:**
```haskell
-- ANTES
data TAC = ...

-- DEPOIS
data Instr = ...
```

---

#### 2. Renomear Funções de Geração TAC

**Fonte:** 
- Aula Teórica 10: `transExpr`, `transStm`
- Prática 6: `compileExpr`, `compileCmd`

**Decisão:** Usar nomenclatura da Prática 6 (mais comum em compiladores)

**Arquivos afetados:**
- `TAC.hs`
- `Main.hs`

**Mudanças:**
```haskell
-- ANTES
generateExpr :: Expr -> TACState -> ([TAC], String, TACState)
generateStmt :: Stmt -> TACState -> ([TAC], TACState)
generateStmtList :: [Stmt] -> TACState -> ([TAC], TACState)
generateTAC :: Program -> [TAC]

-- DEPOIS
compileExpr :: Expr -> TACState -> ([Instr], String, TACState)
compileStmt :: Stmt -> TACState -> ([Instr], TACState)
compileStmtList :: [Stmt] -> TACState -> ([Instr], TACState)
compile :: Program -> [Instr]
```

**Alternativa para Main:**
```haskell
-- Manter nome genérico no Main
compileTAC :: Program -> [Instr]  -- mais claro que só "compile"
```

---

#### 3. Renomear Função MIPS

**Fonte:** Prática 6, pergunta 4
```
"Defina em Haskell uma função printMIPS que imprime num ficheiro..."
```

**Arquivos afetados:**
- `MIPS.hs`
- `Main.hs`

**Mudança:**
```haskell
-- ANTES
generateMIPS :: [TAC] -> String

-- DEPOIS
printMIPS :: [Instr] -> String
```

---

#### 4. Renomear Construtores de `Instr` (Maiúsculas)

**Fonte:** Aula Teórica 10
- `LABEL` (não `Label`)
- `JUMP` (não `Goto`)

**Arquivos afetados:**
- `AST.hs`
- `TAC.hs`
- `MIPS.hs`

**Mudanças:**
```haskell
-- ANTES
data TAC =
    Assign String String
  | BinOp String String String String
  | UnOp String String String
  | Goto String
  | Ifz String String
  | Label String

-- DEPOIS  
data Instr =
    Assign String String
  | BinOp String String String String
  | UnOp String String String
  | JUMP String           -- Goto → JUMP
  | Ifz String String     -- Manter (slide usa COND, mas Ifz é mais específico)
  | LABEL String          -- Label → LABEL
```

**NOTA sobre Ifz vs COND:**
- Slide: `COND temp relop Atom label label` (dois labels)
- Implementação: `Ifz String String` (condição, um label)
- **Decisão:** Manter `Ifz` pois é mais específico e funcionalmente correto

---

### ⚠️ PRIORIDADE MÉDIA - Adicionar Comentários e Documentação

#### 5. Adicionar Comentários Bilíngues em AST.hs

**Arquivos afetados:**
- `AST.hs`

**Mudanças:**
```haskell
-- Symbol Table Operations (Operações sobre tabela de símbolos - Aula 8)

-- Criar tabela vazia (inicializar)
emptySymbolTable :: SymbolTable

-- Inserir símbolo
insertSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable

-- Procurar símbolo (lookup)
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo

-- Abrir âmbito (enter scope)
enterScope :: SymbolTable -> SymbolTable

-- Fechar âmbito (exit scope)
exitScope :: SymbolTable -> SymbolTable
```

---

#### 6. Adicionar Flags Detalhadas no Código

**Arquivos afetados:**
- `TAC.hs`
- `MIPS.hs`

**Flags a adicionar:**

**Em TAC.hs:**
```haskell
-- 🚩 EXTENSÃO: Operador Unário
-- Prática 6 define apenas: "VAR := atom binop atom"
-- Ada requer operadores unários (-, not)
-- Fonte implícita: Gramática Ada
data Instr = ...
  | UnOp String String String  -- x := op y
  ...
```

**Em MIPS.hs:**
```haskell
-- 🚩 OTIMIZAÇÃO NÃO REQUERIDA: Constant folding
-- Não mencionado em Aula 10, 12 ou Prática 6
-- Justificativa: Otimização básica, melhora eficiência
if isImmediate src1 && isImmediate src2
then let result = show ((read src1 :: Int) + (read src2 :: Int))
     in "  li " ++ destReg ++ ", " ++ result
...
```

**Em TAC.hs (TACState):**
```haskell
-- 📝 DECISÃO DE IMPLEMENTAÇÃO: Estado explícito
-- Prática 6 sugere: compileExpr :: Expr -> ([Instr], Temp)
-- Implementação usa threading explícito de estado (estilo funcional)
-- Alternativas: State monad ou variáveis globais
-- Escolha: Explícito, puro, sem monads
data TACState = TACState {
    tempCount :: Int,
    labelCount :: Int
}
```

---

### ℹ️ PRIORIDADE BAIXA - Melhorias Opcionais

#### 7. Criar Aliases para Compatibilidade (Opcional)

**Arquivos afetados:**
- `AST.hs`

**Mudanças opcionais:**
```haskell
-- Aliases para compatibilidade exata com slides (termos em português)
procurar :: String -> SymbolTable -> Maybe SymbolInfo
procurar = lookupSymbol

abrir :: SymbolTable -> SymbolTable
abrir = enterScope

fechar :: SymbolTable -> SymbolTable
fechar = exitScope
```

**NOTA:** Não obrigatório - código em inglês é aceitável em Haskell

---

#### 8. Atualizar Documentação

**Arquivos afetados:**
- `TRABALHO_PARTE2_DOCUMENTATION.md`
- `PARTE2_RESUMO_EXECUTIVO.md`
- `SOURCES_DETAILED.md`

**Mudanças:**
- Atualizar referências `TAC` → `Instr`
- Atualizar referências `generateExpr` → `compileExpr`
- Adicionar nota sobre alinhamento com nomenclatura dos slides

---

## 🔄 Ordem de Execução Recomendada

### Fase 1 - Refactoring de Tipos (Requer build após cada passo)

1. **AST.hs:** Renomear `data TAC` → `data Instr`
2. **TAC.hs:** Atualizar todas as referências `TAC` → `Instr`
3. **MIPS.hs:** Atualizar referências `TAC` → `Instr`
4. **Main.hs:** Atualizar referências `TAC` → `Instr`
5. **Build e test:** `make clean && make && ./Main test.ada`

### Fase 2 - Refactoring de Funções (Requer build)

6. **TAC.hs:** Renomear funções `generate*` → `compile*`
7. **MIPS.hs:** Renomear `generateMIPS` → `printMIPS`
8. **Main.hs:** Atualizar chamadas às funções
9. **Build e test:** `make clean && make && ./Main test.ada`

### Fase 3 - Refactoring de Construtores (Requer build)

10. **AST.hs:** Renomear `Label` → `LABEL`, `Goto` → `JUMP`
11. **TAC.hs:** Atualizar pattern matching
12. **MIPS.hs:** Atualizar pattern matching
13. **Build e test:** `make clean && make && ./Main test.ada`

### Fase 4 - Documentação (Não requer build)

14. **AST.hs, TAC.hs, MIPS.hs:** Adicionar comentários bilíngues e flags
15. **Documentação MD:** Atualizar referências
16. **Git commit:** Commit final com mensagem apropriada

---

## ✅ Checklist de Validação

Após cada fase, verificar:

- [ ] Código compila sem erros: `make clean && make`
- [ ] Testes passam: `./Main test_comprehensive_pr3.ada`
- [ ] TAC gerado está correto (visualmente)
- [ ] MIPS gerado está correto (visualmente)
- [ ] Arquivo `.asm` é criado
- [ ] Documentação menciona fontes corretas

---

## 🚨 Riscos e Mitigações

| Risco | Probabilidade | Impacto | Mitigação |
|-------|---------------|---------|-----------|
| Erro de sintaxe ao renomear | Baixa | Alto | Build após cada mudança |
| Pattern matching incompleto | Média | Alto | Usar `-Wall` no GHC |
| Referências perdidas | Baixa | Médio | Usar `grep -r "TAC"` antes de commitar |
| Documentação desatualizada | Alta | Baixo | Atualizar docs na Fase 4 |

---

## 📝 Notas Importantes

### Sobre `Ifz` vs `COND`

O slide mostra:
```
COND temp relop Atom label label
```

A implementação usa:
```haskell
Ifz String String  -- "if zero goto label"
```

**Decisão:** Manter `Ifz` porque:
1. É mais específico (testa se zero)
2. Simplifica a geração de código
3. Funcionalmente equivalente (COND pode ser decomposto em Ifz)
4. Common pattern em compiladores

**Flag recomendada:**
```haskell
-- 📝 DECISÃO: Ifz vs COND
-- Slide usa: COND temp relop Atom label label
-- Implementação: Ifz cond label (mais específico)
-- Justificativa: Simplifica geração, padrão comum em compiladores
| Ifz String String
```

### Sobre Threading de Estado

Prática 6 sugere:
```haskell
compileExpr :: Expr -> ([Instr], Temp)
```

Implementação usa:
```haskell
compileExpr :: Expr -> TACState -> ([Instr], String, TACState)
```

**Decisão:** Manter threading explícito porque:
1. É puro e explícito
2. Evita monads (mais simples para iniciantes)
3. Funciona perfeitamente
4. Não é erro - apenas diferente estilo

---

## 🎓 Resumo para Avaliação

**Após refactoring, o código estará:**
- ✅ Alinhado com nomenclatura EXATA dos slides
- ✅ Usando `Instr` (não `TAC`)
- ✅ Usando `compileExpr` (não `generateExpr`)
- ✅ Usando `printMIPS` (não `generateMIPS`)
- ✅ Usando `LABEL`, `JUMP` (maiúsculas)
- ✅ Com comentários bilíngues referenciando slides
- ✅ Com flags claras para extensões
- ✅ Funcionalidade intacta

**Pode demonstrar alinhamento com materiais:**
- Código fonte com nomenclatura dos slides
- Comentários citando aulas específicas
- Flags claras para extensões justificadas
- Documentação completa e atualizada
