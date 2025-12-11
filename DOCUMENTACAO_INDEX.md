# Documentação do Compilador Ada - Índice Principal

## Estrutura da Documentação

Este projeto contém documentação completa sobre o desenvolvimento do compilador Ada em Haskell.

---

## Documentos Disponíveis

### 1. **TRABALHO_PARTE2_DOCUMENTATION.md** (30 KB, 1085 linhas)
**Documentação técnica completa e detalhada da segunda parte**

Contém:
- Comparação completa de código (antes vs depois)
- Todo o código fonte dos módulos novos (Semantic.hs, TAC.hs, MIPS.hs)
- Análise linha-a-linha das mudanças em AST.hs e Main.hs
- Referências específicas a:
  - Aulas Teóricas (8, 9, 10, 12)
  - Aulas Práticas (5, 6)
  - Bibliografia (Saumya Debray)
- Explicação detalhada de cada conceito aplicado
- Exemplos de transformações de código
- Flags para código sem fonte direta nas aulas

**Recomendado para:** Análise técnica profunda, compreensão de implementação

---

### 2. **PARTE2_RESUMO_EXECUTIVO.md** (12 KB, 345 linhas)
**Resumo executivo e visão geral**

Contém:
- Resumo de todas as mudanças
- Estatísticas de código
- Tabelas comparativas (TAC → MIPS)
- Lista de funcionalidades implementadas
- Referências organizadas por fonte
- Casos de teste cobertos
- Funcionalidades não implementadas
- Próximos passos sugeridos

**Recomendado para:** Visão rápida, apresentações, revisão geral

---

### 3. **README_INTEGRATION.md** (4.2 KB)
**Guia de integração e uso**

Contém:
- Como compilar o projeto
- Como executar o compilador
- Exemplos de uso
- Pipeline do compilador
- Formato dos ficheiros de saída

**Recomendado para:** Uso prático, setup inicial

---

## Organização por Tópico

### Análise Semântica
- **Documento principal:** TRABALHO_PARTE2_DOCUMENTATION.md, seção 4
- **Resumo:** PARTE2_RESUMO_EXECUTIVO.md, "Semantic.hs"
- **Módulo:** `Semantic.hs` (102 linhas)
- **Fontes:** Aula Teórica 8

### Código Intermédio (TAC)
- **Documento principal:** TRABALHO_PARTE2_DOCUMENTATION.md, seção 5
- **Resumo:** PARTE2_RESUMO_EXECUTIVO.md, "TAC.hs"
- **Módulo:** `TAC.hs` (175 linhas)
- **Fontes:** Aula Teórica 10, Prática 5, Prática 6

### Código Assembly (MIPS)
- **Documento principal:** TRABALHO_PARTE2_DOCUMENTATION.md, seção 6
- **Resumo:** PARTE2_RESUMO_EXECUTIVO.md, "MIPS.hs"
- **Módulo:** `MIPS.hs` (277 linhas)
- **Fontes:** Aula Teórica 12, Bibliografia (Debray)

### Tabelas de Símbolos
- **Documento principal:** TRABALHO_PARTE2_DOCUMENTATION.md, seção 3.2.3-3.2.7
- **Resumo:** PARTE2_RESUMO_EXECUTIVO.md, "AST.hs"
- **Módulo:** `AST.hs` (operações de tabela de símbolos)
- **Fontes:** Aula Teórica 8

---

## Guia de Leitura Recomendado

### Para Compreensão Rápida:
1. Ler **PARTE2_RESUMO_EXECUTIVO.md** completo (15-20 min)
2. Ver exemplos em **README_INTEGRATION.md** (5 min)

### Para Análise Detalhada:
1. Ler **PARTE2_RESUMO_EXECUTIVO.md** para contexto
2. Ler **TRABALHO_PARTE2_DOCUMENTATION.md** seção por seção
3. Consultar código fonte dos módulos em paralelo

### Para Desenvolvimento/Manutenção:
1. **README_INTEGRATION.md** - Setup e compilação
2. **TRABALHO_PARTE2_DOCUMENTATION.md** - Compreensão da arquitetura
3. Código fonte comentado

---

## Referências Principais dos Materiais do Curso

### Aulas Teóricas:
- **Aula 8** - Análise semântica e tabelas de símbolos → `Semantic.hs`, `AST.hs`
- **Aula 9** - Sistemas de tipos → `AST.hs` (Type)
- **Aula 10** - Geração de código intermédio → `TAC.hs`
- **Aula 12** - Emissão de código assembler → `MIPS.hs`

### Aulas Práticas:
- **Prática 5** - pratica5_3address.pdf → Formato TAC
- **Prática 6** - pratica6_codeGen_hs.txt → Estrutura de compilação em Haskell

### Bibliografia:
- **Saumya Debray** - "Notes on Translating 3-address Code to MIPS Assembly Code"
  - Referência explícita no código: `MIPS.hs` linha 4
  - Usado para: Convenções MIPS, tradução de instruções

---

## Estrutura do Projeto

```
Compilador/
├── TRABALHO_PARTE2_DOCUMENTATION.md  ← Documentação completa
├── PARTE2_RESUMO_EXECUTIVO.md        ← Resumo executivo
├── README_INTEGRATION.md              ← Guia de uso
├── AST.hs                             ← Árvore sintática + Tabelas símbolos + TAC
├── Semantic.hs                        ← Análise semântica (NOVO)
├── TAC.hs                             ← Código intermédio (NOVO)
├── MIPS.hs                            ← Código assembly (NOVO)
├── Main.hs                            ← Integração
├── Parser.y                           ← Parser (estendido)
├── Lexer.x                            ← Lexer (mantido)
├── Token.hs                           ← Tokens (mantido)
└── test*.ada                          ← Ficheiros de teste
```

---

## Estatísticas Gerais

### Código da Segunda Parte:
- **Total de linhas:** 923
- **Novos módulos:** 3 (Semantic, TAC, MIPS)
- **Módulos alterados:** 3 (AST, Main, Parser)
- **Funções novas:** ~35
- **Tipos de dados novos:** 6

### Documentação:
- **Total de páginas:** ~45 (em formato impresso)
- **Palavras:** ~20,000
- **Exemplos de código:** ~50
- **Referências a aulas:** 8 (4 teóricas, 2 práticas, 1 bibliografia, 1 material suplementar)

---

## Validação e Testes

### Ficheiros de Teste Incluídos:
- `test_declarations.ada` - Declarações básicas
- `test_redeclaration.ada` - Erro: redeclaração
- `test_undeclared.ada` - Erro: variável não declarada  
- `test_nested_scope.ada` - Escopos aninhados
- `test_arithmetic_decl.ada` - Operações aritméticas
- `test_comprehensive_pr2.ada` - Teste completo parte 2
- `test_comprehensive_pr3.ada` - Teste completo parte 3

### Cobertura:
- ✓ Análise léxica e sintática
- ✓ Análise semântica (declarações, uso, escopos)
- ✓ Geração de TAC
- ✓ Geração de MIPS
- ✓ Execução em simulador (MARS/SPIM)

---

## Como Usar Esta Documentação

### Cenário 1: Preciso compreender o que foi feito
→ Leia **PARTE2_RESUMO_EXECUTIVO.md**

### Cenário 2: Preciso verificar as fontes dos conceitos
→ Leia **TRABALHO_PARTE2_DOCUMENTATION.md**, procure por "**Fonte:**"

### Cenário 3: Preciso executar o compilador
→ Leia **README_INTEGRATION.md**

### Cenário 4: Preciso compreender uma função específica
→ Busque o nome da função em **TRABALHO_PARTE2_DOCUMENTATION.md**

### Cenário 5: Preciso ver todo o código de um módulo
→ **TRABALHO_PARTE2_DOCUMENTATION.md** contém código completo de:
  - Semantic.hs (seção 4.1)
  - TAC.hs (seção 5.1)
  - AST.hs (seção 3.1)

---

## Flags e Observações Especiais

Os documentos incluem **🚩 FLAGS** para marcar:
- Implementações sem fonte direta nas aulas
- Decisões de design próprias
- Otimizações adicionais
- Funcionalidades preparadas mas não implementadas

Exemplo de flag:
```
🚩 FLAG: Constant folding não foi explicitamente requerido,
mas é uma otimização básica e natural de implementar.
```

---

## Contato e Manutenção

Para questões sobre a documentação ou implementação:
1. Consulte primeiro os documentos relevantes
2. Verifique as referências às aulas citadas
3. Analise o código fonte comentado

---

## Versão da Documentação

- **Versão:** 1.0
- **Data:** 11 de Dezembro de 2025
- **Autor:** Grupo 3 - CC3001
- **Compilador:** Subconjunto Ada → MIPS Assembly

---

## Licença e Uso Acadêmico

Esta documentação foi criada como parte do Trabalho Prático de Compiladores (CC3001) da Faculdade de Ciências da Universidade do Porto.

Todos os conceitos e técnicas implementadas seguem os materiais do curso ministrado no ano letivo 2025/2026.
