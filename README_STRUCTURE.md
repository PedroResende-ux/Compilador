# Estrutura do Projeto - Compilador Ada

## 📁 Organização de Pastas

```
Compilador/
│
├── 📂 para_submeter/               # CÓDIGO FONTE (para submeter)
│   ├── Lexer.x                     # Análise léxica
│   ├── Parser.y                    # Análise sintática
│   ├── AST.hs                      # Árvore sintática + Symbol Table
│   ├── Token.hs                    # Definições de tokens
│   ├── Semantic.hs                 # Análise semântica
│   ├── TAC.hs                      # Geração de código intermédio
│   ├── MIPS.hs                     # Geração de código MIPS
│   ├── Main.hs                     # Programa principal
│   ├── Makefile                    # Build do projeto
│   ├── build.sh                    # Script de build alternativo
│   └── run_tests.sh                # Script para rodar testes
│
├── 📂 testes/                      # TESTES (para submeter)
│   ├── test.ada                    # Teste básico
│   ├── test_comprehensive.ada      # Teste completo
│   ├── test_arithmetic_decl.ada    # Teste aritmética
│   ├── test_declarations.ada       # Teste declarações
│   ├── test_nested_scope.ada       # Teste escopos aninhados
│   ├── test_undeclared.ada         # Teste erro: variável não declarada
│   ├── test_redeclaration.ada      # Teste erro: redeclaração
│   ├── test_comprehensive_pr2.ada  # Teste PR2
│   ├── test_comprehensive_pr3.ada  # Teste PR3
│   ├── test.file.with.dots.ada     # Teste nomes com pontos
│   └── demo_integration.ada        # Demo de integração
│
├── 📂 lixo/                        # ARTEFATOS DE COMPILAÇÃO (NÃO submeter)
│   ├── *.hi                        # Interface files (Haskell)
│   ├── *.o                         # Object files
│   ├── Lexer.hs                    # Gerado do Lexer.x
│   ├── Parser.hs                   # Gerado do Parser.y
│   ├── compilador                  # Executável compilado
│   └── *.asm                       # Ficheiros assembly gerados
│
├── 📂 documentacao/                # DOCUMENTAÇÃO DETALHADA (referência)
│   ├── README.md                   # ⭐ Instruções principais
│   ├── README_INTEGRATION.md       # Explicação da integração PRs
│   ├── COMPARACAO_PARTE1_PARTE2.md # Comparação entre partes
│   ├── TRABALHO_PARTE2_DOCUMENTATION.md # Doc técnica completa
│   ├── QUICK_REFERENCE.md          # Referência rápida
│   ├── FLAGS_E_FONTES_RESUMO.md    # Resumo de fontes
│   └── [outros...]                 # Documentação auxiliar
│
└── 📂 ferramentas/                 # PREPARAÇÃO PARA SUBMISSÃO (vazia)
    └── (usar para montar o .zip final)
```

---

## 🎯 Guia Rápido

### **Para Compilar:**
```bash
cd para_submeter/
make
# Gera: ../lixo/compilador
```

### **Para Testar:**
```bash
cd para_submeter/
./run_tests.sh
# Ou manualmente:
../lixo/compilador ../testes/test_comprehensive.ada
```

### **Para Limpar:**
```bash
cd para_submeter/
make clean
# Remove todos os ficheiros em lixo/
```

---

## 📦 Para Submeter

### **O que incluir no .zip:**

**✅ OBRIGATÓRIO:**
- `para_submeter/` (todo o conteúdo)
- `testes/` (todo o conteúdo)
- `README.pdf` (converter `documentacao/README.md` para PDF)

**❌ NÃO INCLUIR:**
- `lixo/` (ficheiros gerados)
- `documentacao/` (documentação extra)
- `.git/` (histórico git)
- `.DS_Store` (metadata MacOS)

### **Estrutura do .zip final:**
```
COMP_TP2_GRUPO_<numero>.zip
├── README.pdf
├── Lexer.x
├── Parser.y
├── AST.hs
├── Token.hs
├── Semantic.hs
├── TAC.hs
├── MIPS.hs
├── Main.hs
├── Makefile
├── build.sh
├── run_tests.sh
└── tests/
    └── [todos os ficheiros .ada]
```

---

## 🚀 Script para Preparar Submissão

Execute isto para criar o .zip automaticamente:

```bash
# Na raiz do projeto
cd /Users/pedroresende/Documents/GitHub/Compilador

# Limpa lixo
cd para_submeter && make clean && cd ..

# Copia ficheiros para ferramentas/
cp -r para_submeter/* ferramentas/
cp -r testes ferramentas/

# Converte README para PDF (requer pandoc)
pandoc documentacao/README.md -o ferramentas/README.pdf

# Remove ficheiros desnecessários
rm -rf ferramentas/lixo ferramentas/.DS_Store

# Cria o .zip
cd ferramentas
zip -r ../COMP_TP2_GRUPO_XX.zip .
cd ..

echo "✅ Ficheiro COMP_TP2_GRUPO_XX.zip criado!"
```

---

## 💻 Para a Apresentação

### **Ter Pronto no Computador:**

1. **Compilador compilado:**
   ```bash
   cd para_submeter && make
   ```

2. **Exemplos testados:**
   ```bash
   cd para_submeter
   ../lixo/compilador ../testes/test_comprehensive.ada
   cat ../lixo/test_comprehensive.asm
   ```

3. **Abrir no VS Code:**
   - `para_submeter/Main.hs` - mostra pipeline
   - `para_submeter/Semantic.hs` - análise semântica
   - `para_submeter/TAC.hs` - código intermédio
   - `para_submeter/MIPS.hs` - geração assembly

4. **Ter MARS pronto** (se for demonstrar execução)

---

## 📚 Documentação

- **README Principal:** `documentacao/README.md`
- **Documentação Técnica Completa:** `documentacao/TRABALHO_PARTE2_DOCUMENTATION.md`
- **Referência Rápida:** `documentacao/QUICK_REFERENCE.md`
- **Integração PRs:** `documentacao/README_INTEGRATION.md`

---

## ✅ Checklist Final

- [ ] Código compila sem erros (`cd para_submeter && make`)
- [ ] Testes passam (`cd para_submeter && ./run_tests.sh`)
- [ ] README.pdf criado
- [ ] .zip montado e testado
- [ ] Ficheiros `.hi`, `.o`, executáveis removidos do .zip
- [ ] Compilador funcionando no computador para apresentação
- [ ] Exemplos .asm gerados para demonstrar
