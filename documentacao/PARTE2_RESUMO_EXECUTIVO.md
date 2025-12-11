# Resumo Executivo - Segunda Parte do Trabalho Prático

## Visão Geral

Este documento complementa o documento principal (`TRABALHO_PARTE2_DOCUMENTATION.md`) com um resumo executivo das mudanças implementadas na segunda parte do compilador Ada.

**📚 Todas as fontes foram verificadas via pdftotext dos PDFs do curso (11 Dez 2025)**

Ver `SOURCES_DETAILED.md` para mapeamento detalhado de cada implementação às fontes específicas com citações diretas e números de linha dos PDFs.

---

## Ficheiros Criados/Alterados

### Novos Módulos

1. **Semantic.hs** (102 linhas) - Análise Semântica
2. **TAC.hs** (175 linhas) - Geração de Código Intermédio
3. **MIPS.hs** (277 linhas) - Geração de Assembly MIPS

### Módulos Alterados

1. **AST.hs** - Expandido de 48 para 154 linhas (+221%)
2. **Main.hs** - Expandido de 148 para 215 linhas (+45%)
3. **Parser.y** - Adicionado suporte a declarações de variáveis

---

## Mudanças por Módulo

### 1. AST.hs - Alterações Fundamentais

#### Adições Principais:
- Sistema de tipos (`Type = IntegerType | BooleanType`)
- Tabela de símbolos (`SymbolTable`, `SymbolInfo`, `Scope`)
- Estrutura TAC (6 tipos de instruções)
- Definição de declarações (`Decl = VarDecl String Type`)
- 6 operações sobre tabelas de símbolos

#### Fonte Principal:
- **Aula Teórica 8** - Análise semântica e tabelas de símbolos
- **Aula Teórica 10** - Geração de código intermédio
- **Aula Prática 5 e 6** - TAC e code generation

#### Mudança Crítica:
```haskell
-- ANTES:
data Program = Program [Stmt]

-- DEPOIS:
data Program = Program [Decl] [Stmt]
```

**Justificação:** Separação explícita entre declarações e comandos, conforme semântica de Ada.

---

### 2. Semantic.hs - Análise Semântica Completa

#### Funcionalidades Implementadas:

1. **Verificação de Declarações**
   - Detecção de redeclarações no mesmo escopo
   - Construção da tabela de símbolos

2. **Verificação de Uso de Variáveis**
   - Detecção de variáveis não declaradas
   - Verificação recursiva em expressões

3. **Suporte a Escopos Aninhados**
   - Entrada/saída de escopos para blocos `begin...end`
   - Shadowing correto de variáveis

4. **Acumulação de Erros**
   - Reporta todos os erros encontrados
   - Não para na primeira ocorrência

#### Estrutura de Dados Principal:
```haskell
data SemanticResult = SemanticResult
  { errors :: [String]        -- Erros encontrados
  , warnings :: [String]      -- Avisos
  , symbolTable :: SymbolTable -- Tabela final
  }
```

#### Fonte Principal:
- **Aula Teórica 8** - Todas as técnicas de análise semântica
- Enunciado do Trabalho Prático - Segunda Parte

#### Casos de Erro Detectados:

1. **Redeclaração:**
   ```ada
   declare
     x : Integer;
     x : Boolean;  -- ERRO
   ```

2. **Variável Não Declarada:**
   ```ada
   declare
     x : Integer;
   begin
     y := 5;  -- ERRO: y não declarado
   ```

---

### 3. TAC.hs - Código Intermédio

#### Funcionalidades:

1. **Geração de Temporários**
   - Contadores para nomes únicos (t0, t1, t2, ...)
   - Estado threading funcional

2. **Geração de Labels**
   - Contadores para labels únicos (L0, L1, L2, ...)
   - Usados para controlo de fluxo

3. **Compilação de Expressões**
   - Linearização de expressões complexas
   - Decomposição em operações atômicas
   - Preservação de ordem de avaliação

4. **Compilação de Comandos**
   - Assignment, If-Then-Else, If-Then, While, Block
   - PutLine (I/O), GetLine (Input)

#### Exemplo de Transformação:

**Código Ada:**
```ada
declare
  x, y, z : Integer;
begin
  z := (x + y) * 2;
end;
```

**TAC Gerado:**
```
t0 = x + y
t1 = t0 * 2
z = t1
```

#### Fonte Principal:
- **Aula Teórica 10** - Geração de código intermédio
- **Aula Prática 6** - "pratica6_codeGen_hs.txt" (referência direta)
- **Aula Prática 5** - "pratica5_3address.pdf"

#### Padrões de Compilação:

**If-Then-Else:**
```
<código condição>
ifz <cond> goto else_label
<código then>
goto end_label
else_label:
<código else>
end_label:
```

**While:**
```
start_label:
<código condição>
ifz <cond> goto end_label
<código corpo>
goto start_label
end_label:
```

---

### 4. MIPS.hs - Código Assembly

#### Funcionalidades:

1. **Alocação de Registos**
   - Variáveis → $s0-$s7 (saved registers)
   - Temporários → $t0-$t9 (temporary registers)
   - Mapeamento estático baseado em uso

2. **Gestão de Strings**
   - Extração de literais string
   - Geração de seção `.data`
   - Labels automáticos (str0, str1, ...)

3. **Tradução de Instruções TAC → MIPS**
   - Operações aritméticas (add, sub, mul, div, mod)
   - Operações lógicas (and, or, not)
   - Comparações (eq, neq, lt, lte, gt, gte)
   - Controlo de fluxo (goto, labels, jumps condicionais)

4. **Syscalls**
   - Print integer (syscall 1)
   - Print string (syscall 4)
   - Read integer (syscall 5)
   - Exit (syscall 10)

#### Fonte Principal:
- **Aula Teórica 12** - Emissão de código assembler
- **Bibliografia: Saumya Debray** - "Notes on Translating 3-address Code to MIPS Assembly Code" (referência explícita no código)
- **Aula Prática 6** - Code generation para MIPS

#### Mapeamento TAC → MIPS:

| TAC | MIPS | Observação |
|-----|------|------------|
| `t0 = a + b` | `add $t0, $s0, $s1` | Soma de registos |
| `t0 = a + 5` | `addi $t0, $s0, 5` | Soma com imediato |
| `t0 = 3 + 5` | `li $t0, 8` | Constant folding |
| `t0 = a / b` | `div $s0, $s1`<br>`mflo $t0` | Divisão inteira |
| `t0 = a mod b` | `div $s0, $s1`<br>`mfhi $t0` | Resto da divisão |
| `goto L0` | `j L0` | Salto incondicional |
| `ifz t0 goto L0` | `beqz $t0, L0` | Salto se zero |
| `L0:` | `L0:` | Label |

#### Exemplo Completo:

**TAC:**
```
t0 = x + 5
y = t0
```

**MIPS Gerado:**
```mips
  addi $t0, $s0, 5    # t0 = x + 5
  move $s1, $t0       # y = t0
```

#### Otimizações Implementadas:

1. **Constant Folding** - Avaliação de constantes em tempo de compilação
2. **Uso de instruções imediatas** - `addi`, `slti` quando possível
3. **Reutilização de registos temporários** - Seguindo convenção $t0-$t9

**🚩 FLAG:** Constant folding não foi explicitamente requerido, mas é uma otimização básica.

---

### 5. Main.hs - Integração

#### Mudanças Principais:

1. **Nova Pipeline do Compilador:**
   ```
   Código Ada → Léxico → Sintático → Semântico → TAC → MIPS
   ```

2. **Adições:**
   - Import de módulos `Semantic`, `TAC`, `MIPS`
   - Chamada a `analyzeProgram`
   - Verificação de erros semânticos (exit se erros)
   - Geração e impressão de TAC
   - Geração e impressão de MIPS
   - Escrita de ficheiro `.asm`

3. **Geração Automática de Nome de Ficheiro:**
   ```haskell
   -- test.ada → test.asm
   let outputFile = withoutExt ++ ".asm"
   ```

4. **Impressão de AST Melhorada:**
   - Suporte a declarações
   - Formatação hierárquica mantida

#### Output do Compilador:

```
=== ABSTRACT SYNTAX TREE ===
<árvore sintática>

=== SEMANTIC ANALYSIS ===
✓ No semantic errors found

=== SYMBOL TABLE ===
<tabela de símbolos>

=== THREE-ADDRESS CODE ===
<código TAC>

=== MIPS ASSEMBLY CODE ===
<código MIPS>

MIPS code written to: test.asm
```

---

## Referências aos Materiais do Curso

### Aulas Teóricas Utilizadas:

1. **Aula 8** - Análise semântica e tabelas de símbolos
   - Estruturas de dados para tabelas
   - Operações de inserção/busca
   - Gestão de escopos
   - Detecção de erros

2. **Aula 9** - Sistemas de tipos
   - Definição de tipos
   - Verificação de tipos (parcialmente implementado)

3. **Aula 10** - Geração de código intermédio
   - Three-address code
   - Compilação de expressões
   - Compilação de comandos
   - Geração de labels e temporários

4. **Aula 12** - Emissão de código assembler
   - Arquitetura MIPS
   - Convenções de registos
   - Instrução de controlo
   - Syscalls

### Aulas Práticas Utilizadas:

1. **Prática 5** - "pratica5_3address.pdf"
   - Formato de TAC
   - Definição de instruções

2. **Prática 6** - "pratica6_codeGen_hs.txt"
   - **REFERÊNCIA DIRETA NO CÓDIGO**
   - Estrutura de compilação em Haskell
   - Tipos para geração de código
   - Exemplo: `compileExpr :: Expr -> ([Instr], Temp)`

### Bibliografia Utilizada:

1. **Saumya Debray** - "Notes on Translating 3-address Code to MIPS Assembly Code"
   - **REFERÊNCIA EXPLÍCITA NO CÓDIGO MIPS.hs (linha 4)**
   - Tradução de TAC para MIPS
   - Convenções de registos
   - Instruções especiais (div/mod)

---

## Flags e Observações

### 🚩 Implementações Não Diretamente Ensinadas:

1. **Constant Folding em MIPS.hs**
   - Otimização básica não explicitamente requerida
   - Implementação: avaliar `3 + 5` em tempo de compilação
   - Justificação: Prática padrão, melhora eficiência

2. **Estrutura `SemanticResult` com Warnings**
   - Campo `warnings` não é usado atualmente
   - Preparado para extensões futuras
   - Boa prática de engenharia de software

3. **Validações Extra em `exitScope` e `insertSymbol`**
   - Checks adicionais para prevenir corrupção de dados
   - Programação defensiva
   - Não explicitamente mencionado nas aulas

4. **Pretty Printing Detalhado**
   - Formatação específica do TAC e AST
   - Baseado em práticas comuns, não em slides específicos

### Funcionalidades NÃO Implementadas:

1. **Verificação Completa de Tipos**
   - Sistema de tipos definido mas não totalmente usado
   - Não verifica compatibilidade (Integer + Boolean)
   - Enunciado não requereu explicitamente

2. **Otimizações Avançadas**
   - Dead code elimination
   - Register coalescing
   - Peephole optimization

3. **Gestão de Memória/Stack**
   - Todas as variáveis em registos
   - Sem suporte a arrays, records
   - Sem frame pointer/stack pointer dinâmico

---

## Estatísticas do Código

### Linhas de Código:

| Módulo | Linhas | Propósito |
|--------|--------|-----------|
| AST.hs | 154 | Estruturas de dados |
| Semantic.hs | 102 | Análise semântica |
| TAC.hs | 175 | Código intermédio |
| MIPS.hs | 277 | Código assembly |
| Main.hs | 215 | Integração |
| **Total** | **923** | **Segunda Parte** |

### Tipos de Dados Definidos:

- 3 em AST.hs (Type, SymbolInfo, SymbolTable)
- 1 em Semantic.hs (SemanticResult)
- 1 em TAC.hs (TACState)
- 1 em MIPS.hs (MIPSState)

### Funções Principais:

- AST.hs: 6 operações de tabela de símbolos
- Semantic.hs: 4 funções de verificação
- TAC.hs: 7 funções de geração
- MIPS.hs: 11 funções de tradução

---

## Testes e Validação

### Ficheiros de Teste:

- `test_declarations.ada` - Declarações básicas
- `test_redeclaration.ada` - Erro de redeclaração
- `test_undeclared.ada` - Erro de variável não declarada
- `test_nested_scope.ada` - Escopos aninhados
- `test_comprehensive_pr2.ada` - Teste completo parte 2
- `test_comprehensive_pr3.ada` - Teste completo parte 3

### Casos de Teste Cobertos:

1. ✓ Declarações de variáveis Integer e Boolean
2. ✓ Detecção de redeclaração
3. ✓ Detecção de uso sem declaração
4. ✓ Escopos aninhados e shadowing
5. ✓ Expressões aritméticas complexas
6. ✓ Expressões booleanas
7. ✓ If-Then-Else
8. ✓ If-Then (sem else)
9. ✓ While loops
10. ✓ Blocos aninhados
11. ✓ Put_Line e Get_Line
12. ✓ Geração de TAC correto
13. ✓ Geração de MIPS funcional

---

## Conclusão

A segunda parte do trabalho implementou com sucesso:

1. **Análise Semântica Completa** - Com tabela de símbolos e detecção de erros
2. **Geração de Código Intermédio** - TAC funcional e bem estruturado
3. **Geração de Código Final** - MIPS assembly executável

Todas as implementações seguem conceitos ensinados nas aulas teóricas e práticas, com referências específicas documentadas. As poucas extensões além do material do curso são otimizações básicas ou boas práticas de engenharia de software.

O compilador resultante é funcional e capaz de compilar programas do subconjunto de Ada especificado, gerando código MIPS executável no simulador MARS ou SPIM.

---

## Próximos Passos (Sugestões)

Para trabalhos futuros ou extensões:

1. Implementar verificação completa de tipos
2. Adicionar suporte a arrays e records
3. Implementar otimizações (dead code, constant propagation)
4. Adicionar suporte a procedures/functions
5. Melhorar alocação de registos (register allocation algorithms)
6. Adicionar geração de código para outras arquiteturas

---

**Documento complementar a:** `TRABALHO_PARTE2_DOCUMENTATION.md` (documentação completa com todo o código)
