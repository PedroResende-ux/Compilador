# Compilador Ada - Trabalho Prático 2
**Compiladores - DCC FCUP**  
**Dezembro 2025**

## Identificação do Grupo
- **Aluno 1:** Pedro Antonio Resende Gulart - up202207418
- **Aluno 2:** Helena Moutinho - up202304719

---

## Descrição do Projeto

Este projeto implementa um **compilador completo para um subconjunto da linguagem Ada**, conforme especificado no enunciado do Trabalho Prático 2. O compilador estende a Parte 1 (Lexer + Parser) com:

1. **Análise Semântica** - Verificação de tipos e tabela de símbolos
2. **Geração de Código Intermédio** - Three-Address Code (TAC)
3. **Geração de Código MIPS** - Assembly MIPS executável

O compilador aceita como entrada um programa Ada válido e produz como saída:
- Ficheiro `.asm` com código assembly MIPS
- Código executável no simulador MARS

---

## Funcionalidades Implementadas

### 📊 Análise Semântica (Semantic.hs)

- **Tabela de Símbolos:** Implementada com `Data.Map` (O(log n))
- **Gestão de Scopes:** Suporte a scopes aninhados (blocos)
- **Verificação de Tipos:** Validação de operações aritméticas e lógicas
- **Detecção de Erros:**
  - Variáveis não declaradas
  - Redeclarações no mesmo scope
  - Incompatibilidade de tipos

### 🔄 Código Intermédio (TAC.hs)

Geração de Three-Address Code com:
- **Instruções:** `Assign`, `BinOp`, `UnOp`, `Label`, `Goto`, `Ifz`
- **Temporários:** Geração automática (`t0`, `t1`, ...)
- **Labels:** Para controlo de fluxo (if/while)
- **Threading explícito de estado:** Gestão de estado funcional pura

### 🖥️ Geração de Código MIPS (MIPS.hs)

- **Alocação de Registos:**
  - Variáveis: `$s0-$s7` (saved registers)
  - Temporários: `$t0-$t9` (temporary registers)
- **Syscalls:** Print (inteiros e strings), input, exit
- **Operações:** Aritméticas, lógicas, relacionais
- **Otimização:** Constant folding para operações com literais
- **Testado no MARS:** https://github.com/dpetersanderson/MARS

---

## Estrutura do Projeto

```
para_submeter/
├── AST.hs              # Definições da AST, tabela de símbolos e TAC
├── Lexer.x             # Analisador léxico (Alex)
├── Parser.y            # Analisador sintático (Happy)
├── Semantic.hs         # Análise semântica
├── TAC.hs              # Gerador de código intermédio
├── MIPS.hs             # Gerador de código MIPS
├── Main.hs             # Pipeline: Lexer→Parser→Semantic→TAC→MIPS
├── Token.hs            # Definições de tokens
├── Makefile            # Automatização da compilação
└── build.sh            # Script alternativo de compilação

testes/
├── test_comprehensive.ada      # Teste completo (todas as funcionalidades)
├── test_declarations.ada       # Teste de declarações
├── test_undeclared.ada         # Teste erro: variável não declarada
├── test_redeclaration.ada      # Teste erro: redeclaração
├── test_nested_scope.ada       # Teste de scopes aninhados
└── ... (11 ficheiros de teste)
```

---

## Requisitos e Instalação

### Pré-requisitos

- **GHC** (Glasgow Haskell Compiler) - versão 8.10 ou superior
- **Alex** - gerador de analisadores léxicos
- **Happy** - gerador de analisadores sintáticos
- **MARS** - simulador MIPS (para executar código gerado)

### Instalação no Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install ghc cabal-install
cabal update
cabal install alex happy
```

### Instalação no macOS (com Homebrew)

```bash
brew install ghc cabal-install
cabal update
cabal install alex happy
```

### Instalação do MARS

```bash
# Download do MARS (Java JAR)
wget https://courses.missouristate.edu/KenVollmar/mars/MARS_4_5_Aug2014/Mars4_5.jar
# ou descarregar manualmente de: https://github.com/dpetersanderson/MARS
```

---

## Como Compilar

### Opção 1: Usando Makefile (Recomendado)

```bash
cd para_submeter/
make
```

### Opção 2: Usando script de build

```bash
cd para_submeter/
./build.sh
```

### Opção 3: Manualmente

```bash
cd para_submeter/
alex Lexer.x
happy Parser.y
ghc --make Main.hs -o compilador
```

---

## Como Executar

### 1. Compilar programa Ada para MIPS

```bash
cd para_submeter/
./compilador ../testes/test_comprehensive.ada
# Saída: test_comprehensive.asm
```

### 2. Executar no MARS

```bash
# Opção A: GUI do MARS
java -jar Mars4_5.jar test_comprehensive.asm

# Opção B: Linha de comandos
java -jar Mars4_5.jar nc test_comprehensive.asm
```

### 3. Passos no MARS (GUI):
1. Abrir o ficheiro `.asm` gerado
2. **Assemblar:** Menu → Run → Assemble (ou F3)
3. **Executar:** Menu → Run → Go (ou F5)
4. Ver output na consola do MARS

---

## Pipeline de Compilação

```
Ficheiro .ada
    ↓
Análise Léxica (Lexer.x)
    ↓ [Tokens]
Análise Sintática (Parser.y)
    ↓ [AST]
Análise Semântica (Semantic.hs)
    ↓ [AST validada + Tabela de Símbolos]
Geração TAC (TAC.hs)
    ↓ [Three-Address Code]
Geração MIPS (MIPS.hs)
    ↓ [Assembly MIPS]
Ficheiro .asm
    ↓
MARS Simulator
    ↓ [Output]
```

---

## Exemplos de Código Gerado

### Programa Ada:

```ada
procedure Main is
begin
  x := 10;
  y := x + 5;
  Put_Line(y)
end Main;
```

### TAC Gerado:

```
x := 10
t0 := x + 5
y := t0
_print(y)
```

### MIPS Gerado (simplificado):

```assembly
.data
newline: .asciiz "\n"

.text
.globl main
main:
  li $s0, 10          # x := 10
  addi $t0, $s0, 5    # t0 := x + 5
  move $s1, $t0       # y := t0
  move $a0, $s1       # print(y)
  li $v0, 1
  syscall
  li $v0, 10          # exit
  syscall
```

---

## Testes Incluídos

### Testes de Sucesso:
- `test_comprehensive.ada` - Programa completo com todas as funcionalidades
- `test_declarations.ada` - Declarações de variáveis
- `test_nested_scope.ada` - Scopes aninhados
- `test_arithmetic_decl.ada` - Operações aritméticas

### Testes de Erro (Semântico):
- `test_undeclared.ada` - Variável não declarada (deve falhar)
- `test_redeclaration.ada` - Redeclaração no mesmo scope (deve falhar)

Para executar todos os testes:
```bash
cd para_submeter/
for f in ../testes/*.ada; do
  echo "Testing $f"
  ./compilador "$f"
done
```

---

## Melhorias em Relação à Parte 1

### Parte 1 (TP1):
- ✅ Análise Léxica (Lexer)
- ✅ Análise Sintática (Parser)
- ✅ Construção da AST

### Parte 2 (TP2) - Adições:
- ✅ **Tabela de Símbolos** com gestão de scopes
- ✅ **Análise Semântica** com verificação de tipos
- ✅ **Declarações de Variáveis** (Integer, Boolean, String)
- ✅ **Geração de TAC** (Three-Address Code)
- ✅ **Geração de MIPS** executável no MARS
- ✅ **Otimização:** Constant folding

---

## Limitações Conhecidas

1. **Estruturas não suportadas:**
   - Arrays
   - Records (structs)
   - Procedures/functions definidas pelo utilizador
   - Parâmetros de funções

2. **Tipos limitados:**
   - Apenas Integer, Boolean e String
   - Sem conversões de tipo

3. **Alocação de registos:**
   - Máximo 8 variáveis em registos `$s0-$s7`
   - Variáveis adicionais vão para a stack (implementação básica)

4. **Entrada de dados:**
   - `Get_Line` implementado apenas para inteiros

---

## Resolução de Problemas

### Erro: "Semantic error: Variable 'x' not declared"
- Adicione declaração antes de usar: `x : Integer;`

### Erro: "Semantic error: Variable 'x' already declared in this scope"
- Variável já foi declarada no scope atual

### Código MIPS não executa no MARS:
- Verificar se há erros de sintaxe no `.asm`
- Garantir que o MARS está configurado para MIPS32

### "alex: command not found" ou "happy: command not found":
```bash
cabal install alex happy
export PATH="$HOME/.cabal/bin:$PATH"
```

---

## Referências

- **MARS Simulator:** https://github.com/dpetersanderson/MARS
- **MIPS Reference:** https://courses.missouristate.edu/KenVollmar/mars/
- **Three-Address Code:** Aulas Teóricas 10-11 (FCUP)
- **Tabela de Símbolos:** Aula Teórica 8 (FCUP)
- **Alex User Guide:** https://www.haskell.org/alex/
- **Happy User Guide:** https://www.haskell.org/happy/

---

## Notas Finais

Este compilador foi desenvolvido como parte do Trabalho Prático 2 da unidade curricular de Compiladores (DCC-FCUP). Implementa um compilador completo desde a análise léxica até a geração de código assembly MIPS executável.

O código gerado foi testado extensivamente no simulador MARS e executa corretamente todos os casos de teste fornecidos.

---

**Data de Submissão:** 11 de Dezembro de 2025  
**Demonstração:** 12/15 de Dezembro de 2025
