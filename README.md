# Compilador Ada (Subset) - CC3001

Compilador para subconjunto da linguagem Ada desenvolvido em Haskell para a UC de Compiladores (FCUP).

## 📁 Estrutura do Repositório

```
Compilador/
├── para_submeter/      # Código fonte para submissão (Parte 2)
│   ├── AST.hs         # Definições da AST e tabela de símbolos
│   ├── Lexer.x        # Analisador léxico (Alex)
│   ├── Parser.y       # Analisador sintático (Happy)
│   ├── Semantic.hs    # Análise semântica
│   ├── TAC.hs         # Geração de código de três endereços
│   ├── MIPS.hs        # Geração de código assembly MIPS
│   ├── Main.hs        # Pipeline principal do compilador
│   ├── Token.hs       # Definições de tokens
│   ├── Makefile       # Build system
│   └── build.sh       # Script de compilação
├── testes/            # Ficheiros de teste .ada
├── documentacao/      # Documentação interna detalhada
├── ferramentas/       # Materiais FCUP (slides, enunciados)
├── lixo/              # Build artifacts (.hi, .o, executáveis)
├── prepare_submission.sh  # Script para criar ZIP de submissão
└── README_STRUCTURE.md    # Documentação da estrutura

```

## 🚀 Como Compilar

```bash
cd para_submeter/
make
# ou
./build.sh
```

## 🧪 Como Executar

```bash
cd para_submeter/
./compilador ../testes/test_comprehensive.ada
# Gera: test_comprehensive.asm
```

Testar no MARS simulator:
1. Abrir o `.asm` gerado no MARS
2. Assemblar (F3)
3. Executar (F5)

## 📋 Submissão (Parte 2)

Para criar o ZIP de submissão:

```bash
./prepare_submission.sh
# Editar número do grupo quando pedido
```

O ZIP conterá:
- Código fonte de `para_submeter/`
- Ficheiros de teste de `testes/`
- README.pdf (documentação)

## 🏗️ Pipeline do Compilador

```
Ficheiro .ada → Lexer → Parser → Semantic → TAC → MIPS → .asm
                (Alex)  (Happy)  (Análise)  (3AC)  (Código)
```

## 📚 Documentação

Ver pasta `documentacao/` para:
- `README.md` - Documentação técnica completa
- `DECISOES_E_FONTES.md` - Decisões de design e fontes
- `QUICK_REFERENCE.md` - Referência rápida
- Comparação Parte 1 vs Parte 2
- E mais...

## 👥 Grupo

Grupo [NÚMERO] - FCUP CC3001 2025/2026

## 📅 Deadlines

- **Parte 2**: 11 Dezembro 2025
- **Demo**: 12/15 Dezembro 2025
