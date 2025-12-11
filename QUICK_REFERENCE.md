# Guia Rápido - Segunda Parte do Compilador Ada

## 📚 Documentos Criados

### 1. **TRABALHO_PARTE2_DOCUMENTATION.md** (30 KB)
**→ Documentação técnica COMPLETA**
- Todo o código fonte comentado
- Comparações antes/depois
- Referências específicas a aulas e slides
- 1085 linhas de documentação detalhada

### 2. **PARTE2_RESUMO_EXECUTIVO.md** (12 KB)  
**→ Resumo executivo e visão geral**
- Tabelas comparativas
- Estatísticas
- Flags e observações
- 345 linhas

### 3. **DOCUMENTACAO_INDEX.md** (7.5 KB)
**→ Índice navegável**
- Como usar a documentação
- Guia de leitura recomendado
- Estrutura do projeto

### 4. **QUICK_REFERENCE.md** (este ficheiro)
**→ Referência rápida**

---

## 🔍 Buscar Informação Rápida

### "Onde está implementada a análise semântica?"
→ **Semantic.hs** (código completo em TRABALHO_PARTE2_DOCUMENTATION.md, seção 4)

### "Como funciona a geração de TAC?"
→ **TAC.hs** (documentação completa em TRABALHO_PARTE2_DOCUMENTATION.md, seção 5)

### "Qual a fonte desta implementação?"
→ Procure por "**Fonte:**" em TRABALHO_PARTE2_DOCUMENTATION.md

### "Quais as mudanças principais?"
→ PARTE2_RESUMO_EXECUTIVO.md, seção "Mudanças por Módulo"

### "Como executar o compilador?"
→ README_INTEGRATION.md

---

## 📋 Resumo Ultra-Rápido

### O que foi feito:
1. ✅ **Semantic.hs** (novo) - Análise semântica completa
2. ✅ **TAC.hs** (novo) - Geração de código intermédio
3. ✅ **MIPS.hs** (novo) - Geração de assembly MIPS
4. ✅ **AST.hs** (expandido) - Tipos, tabelas de símbolos, TAC
5. ✅ **Main.hs** (estendido) - Pipeline completo do compilador

### Linhas de código adicionadas: **923**

### Fontes principais:
- **Aula Teórica 8** → Análise semântica
- **Aula Teórica 10** → TAC
- **Aula Teórica 12** → MIPS
- **Prática 6** → Code generation em Haskell
- **Debray** → TAC para MIPS

---

## 🎯 Funcionalidades Implementadas

### Análise Semântica ✓
- Detecção de redeclarações
- Detecção de variáveis não declaradas  
- Escopos aninhados
- Tabela de símbolos completa

### Código Intermédio (TAC) ✓
- Geração de temporários únicos
- Geração de labels
- Compilação de expressões
- Compilação de comandos (if, while, assign)

### Código Assembly (MIPS) ✓
- Alocação de registos ($s, $t)
- Operações aritméticas e lógicas
- Controlo de fluxo (jumps, labels)
- Syscalls (I/O)
- Gestão de strings

---

## 🏗️ Estrutura da Pipeline

```
Código Ada (.ada)
    ↓
[Lexer.x] → Tokens
    ↓
[Parser.y] → AST
    ↓
[Semantic.hs] → Análise semântica + Tabela de símbolos
    ↓
[TAC.hs] → Three-Address Code
    ↓
[MIPS.hs] → Assembly MIPS (.asm)
```

---

## 📖 Exemplos Rápidos

### TAC para expressão `(a + b) * 2`:
```
t0 = a + b
t1 = t0 * 2
```

### MIPS para `t0 = a + 5`:
```mips
addi $t0, $s0, 5
```

### TAC para `while x > 0 loop x := x - 1 end loop`:
```
L0:
t0 = x > 0
ifz t0 goto L1
t1 = x - 1
x = t1
goto L0
L1:
```

---

## 🚩 Flags Importantes

### Implementações sem fonte direta nas aulas:
1. **Constant folding** - Otimização básica não requerida
2. **SemanticResult.warnings** - Preparado mas não usado
3. **Validações extras** - Programação defensiva
4. **Pretty printing** - Formatação específica

### Não implementado (mas sistema preparado):
1. Verificação completa de tipos
2. Otimizações avançadas
3. Arrays e records
4. Gestão dinâmica de stack

---

## 🔧 Comandos Úteis

### Compilar o compilador:
```bash
make
```

### Executar:
```bash
./Main test.ada
```

### Output gerado:
- `test.asm` - Código MIPS assembly

---

## 📊 Estatísticas

| Métrica | Valor |
|---------|-------|
| Módulos novos | 3 |
| Linhas de código (parte 2) | 923 |
| Linhas de documentação | 2,568 |
| Funções novas | ~35 |
| Testes | 7 ficheiros |
| Aulas referenciadas | 8 |

---

## ✅ Lista de Verificação

- [x] Análise semântica funcional
- [x] Geração de TAC
- [x] Geração de MIPS
- [x] Documentação completa com fontes
- [x] Flags para código sem fonte direta
- [x] Exemplos e testes
- [x] Código comentado
- [x] Referências específicas a slides/aulas

---

## 🎓 Para Avaliação

**Documentos a consultar:**

1. **TRABALHO_PARTE2_DOCUMENTATION.md**
   - Prova de todas as fontes (aulas, práticas, bibliografia)
   - Código completo comentado
   - Flags claros para extensões próprias

2. **PARTE2_RESUMO_EXECUTIVO.md**  
   - Visão geral das mudanças
   - Estatísticas e métricas
   - Casos de teste

3. **Código fonte** (AST.hs, Semantic.hs, TAC.hs, MIPS.hs, Main.hs)
   - Comentários inline
   - Implementação funcional

---

## 📞 Navegação Rápida

**"Quero ver código:"** → TRABALHO_PARTE2_DOCUMENTATION.md, seções 3-6
**"Quero ver estatísticas:"** → PARTE2_RESUMO_EXECUTIVO.md
**"Quero ver fontes:"** → Procure "**Fonte:**" em TRABALHO_PARTE2_DOCUMENTATION.md
**"Quero executar:"** → README_INTEGRATION.md
**"Quero visão geral:"** → Este ficheiro ou DOCUMENTACAO_INDEX.md

---

**Data:** 11 de Dezembro de 2025  
**Projeto:** Compilador Ada → MIPS  
**Curso:** CC3001 - Compiladores  
**Instituição:** FCUP
