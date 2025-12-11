# Decisões de Implementação e Fontes

Este documento lista todas as decisões de implementação tomadas durante o desenvolvimento do compilador, incluindo referências às fontes (aulas, práticas, trabalho prático).

---

## AST.hs

### Symbol Table com Data.Map
**Fonte:** Aula Teórica 8, slide 20
- Recomendação: "Em Haskell: implemente com Data.Map ou Data.HashMap"
- **Escolha:** Data.Map (persistente, adequado para tabelas de símbolos)
- **Complexidade:** O(log n) para inserção, procura, remoção

### Operações da Symbol Table
**Fonte:** Aula Teórica 8, slide 10
- Implementa as 5 operações básicas:
  1. inicializar (emptySymbolTable)
  2. inserir (insertSymbol)
  3. procurar (lookupSymbol)
  4. abrir âmbito (enterScope)
  5. fechar âmbito (exitScope)

### Three-Address Code (Instr)
**Fonte:** Aula Teórica 10, slides 6-7; Prática 6, questão 2
- **Assign:** x := y
- **BinOp:** x := y op z
- **UnOp:** x := op y (EXTENSÃO)
- **Goto:** goto label
- **Ifz:** ifz var label (DECISÃO)
- **Label:** label:

#### EXTENSÃO: UnOp (Operadores Unários)
**Por quê:**
- Aula 10 e Prática 6 focam apenas em operadores binários
- Prática 6, Q2a: "instruction -> VAR := atom binop atom"
- **PROBLEMA:** Ada TEM operadores unários obrigatórios:
  - Negação aritmética: -x
  - Negação lógica: not x
- Gramática do Parser.y inclui: `Expr ::= '-' Expr | 'not' Expr`
- **SEM UnOp seria IMPOSSÍVEL compilar código Ada válido**
- **Implementação:** Análoga a BinOp mas com 1 operando

#### DECISÃO: Ifz vs COND
**Fonte:** Aula 10, slide 11 menciona: `COND temp relop Atom label label`
**Implementação escolhida:** `Ifz (if-zero) + Goto`

**Justificativa:**
- Ifz é forma simplificada comum em TAC (livros de compiladores)
- Expressões booleanas avaliadas como 0/1
- Ifz + Goto = equivalente semântico a COND
- Simplifica geração de código
- Mapeamento: `COND t relop a lt lf ≡ Ifz t lf + Goto lt`

---

## Semantic.hs

### Análise Semântica
**Fonte:** Trabalho Prático Parte 2, requisito 1
- "Build a symbol table with type and any other semantic information"

**Fonte:** Aula Teórica 8
- Conceitos: declaração antes de uso, verificação de redeclarações
- Operações sobre tabela de símbolos (procurar, inserir)

### SemanticResult com campo 'warnings'
**DECISÃO:**
- Trabalho não especifica formato de retorno
- Campo warnings preparado para extensões futuras (variáveis não usadas, etc.)
- Atualmente não utilizado

---

## TAC.hs

### Nomenclatura
**Fonte:** Aula Teórica 10
- Slides usam: `transExpr`, `transStm`, `newTemp`, `newLabel`
- Implementação usa os mesmos nomes para facilitar referência aos slides

### Threading Explícito de Estado (TACState)
**Fonte:** Aula 10, slide 8
- Define newTemp/newLabel como "pseudo-funções não puras"
- "devem retornar variáveis distintas de cada vez"
- **Professor NÃO especifica COMO implementar em Haskell**

**DECISÃO: Threading explícito de estado**
**Escolhas possíveis:**
1. Variáveis globais mutáveis (impuro, não-idiomático)
2. State monad (idiomático mas complexo)
3. **Threading explícito TACState (ESCOLHA)**
4. Reader monad com IORef

**Vantagens:**
- Mantém pureza funcional
- Explícito e fácil de entender
- Sem dependências de bibliotecas monad
- Adequado para iniciantes em Haskell

**Nota:** Prática 6 sugere `compileExpr :: Expr -> ([Instr], Temp)` mas não especifica implementação do estado

---

## MIPS.hs

### Nomenclatura
**Fonte:** Prática 6, questão 4
- Pede: "printMIPS que imprime num ficheiro o código MIPS"
- Implementação: `generateMIPS :: [Instr] -> String`

### MIPSState para Gestão de Registos
**Fonte:** Aula Teórica 12, ponto 1
- "O código máquina tem um número finito de registos"
- Slides mencionam necessidade de alocação mas NÃO especificam algoritmo

**DECISÃO: Alocação estática simples**
- Variáveis do programa → $s0-$s7 (saved registers)
- Temporários TAC → $t0-$t9 (temporary registers)
- Strings → seção .data com labels

**Alternativas possíveis (mencionadas na Aula 14, FORA DO ESCOPO):**
- Graph coloring (algoritmo complexo)
- Linear scan register allocation
- Spilling para stack (quando registos insuficientes)

**Justificativa:**
- Adequado para o subconjunto Ada do trabalho
- MIPS tem registos suficientes ($s0-$s7, $t0-$t9)
- Simples e direto

### Constant Folding (Otimização)
**NOTA: NÃO REQUERIDA**
- Não mencionado em: Aula 10 (geração TAC), Aula 12 (geração MIPS), Trabalho Prático Parte 2
- Aula 15 cobre otimizações mas está FORA DO ESCOPO do trabalho

**Implementação:** Em generateMIPSInstr, quando ambos operandos são constantes
- Exemplo: `t0 := 3 + 5  →  li $t0, 8` (avaliado em compile-time)

**Justificativa para inclusão:**
- Otimização básica natural (comum em todos os compiladores)
- Melhora qualidade do código gerado
- Não altera semântica
- Facilmente removível se necessário (3-4 linhas por operação)

### Seção .data para strings
**Fonte:** Aula 12, exemplo slide 3
- Uso de `.asciiz` para strings

**Fonte:** Aula 10, final
- "Alocação estática: declaramos espaço no segmento de data"

### Syscalls MIPS
**Fonte:** Aula Teórica 12, slides sobre MIPS
- Código 1: print integer
- Código 4: print string
- Código 5: read integer
- Código 10: exit

---

## Referências Completas

### Aulas Teóricas
- **Aula 8:** Análise semântica e tabelas de símbolos
- **Aula 9:** Sistemas de tipos
- **Aula 10:** Geração de código intermédio
- **Aula 12:** Emissão de código assembler
- **Aula 14:** Alocação de registos (mencionada, fora do escopo)
- **Aula 15:** Otimizações (mencionada, fora do escopo)

### Práticas
- **Prática 5:** Geração de código intermédio (exercícios 1-4)
- **Prática 6:** Geração de código (questões 2-4)

### Trabalho Prático - Segunda Parte
1. "Build a symbol table with type and any other semantic information"
2. "Implement an intermediate code generator... three address code"
3. "Implement a code generator for MIPS which, given a list of three-address instructions, prints... its corresponding MIPS code"

### Outras Referências
- **Debray paper:** "Notes on Translating from 3-Address Code to MIPS Assembly" (referenciado na Prática 6, questão 4)
- **Livros de compiladores:** Para conceitos standard como TAC, Ifz, constant folding

---

## Resumo de Decisões Principais

| Aspecto | Decisão | Fonte | Justificativa |
|---------|---------|-------|---------------|
| Symbol Table | Data.Map | Aula 8 | O(log n), recomendado pelo professor |
| Threading Estado TAC | Explícito | Haskell puro | Simples, funcional, sem monads |
| UnOp em TAC | Incluído | Ada syntax | Necessário para `-x` e `not x` |
| Ifz vs COND | Ifz | Simplificação | Equivalente semântico, mais simples |
| Alocação Registos | Estática | Escopo limitado | Suficiente para subset Ada |
| Constant Folding | Incluído | Otimização básica | Melhora código, não altera semântica |

---

**Nota:** Este documento serve de referência interna para o grupo. Os ficheiros submetidos têm comentários simplificados e profissionais, sem flags ou justificativas excessivas.
