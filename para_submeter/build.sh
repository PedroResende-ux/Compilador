#!/bin/bash
# Script de compilação para o compilador Ada

set -e  # Sair em caso de erro

echo "=== Compilador Ada - Build Script ==="
echo ""

# Verificar se Alex está instalado
if ! command -v alex &> /dev/null; then
    echo "ERRO: Alex não está instalado!"
    echo "Instale com: cabal install alex"
    exit 1
fi

# Verificar se Happy está instalado
if ! command -v happy &> /dev/null; then
    echo "ERRO: Happy não está instalado!"
    echo "Instale com: cabal install happy"
    exit 1
fi

# Verificar se GHC está instalado
if ! command -v ghc &> /dev/null; then
    echo "ERRO: GHC não está instalado!"
    echo "Instale o Haskell Platform ou GHC"
    exit 1
fi

echo "1. Gerando analisador léxico com Alex..."
alex Lexer.x

echo "2. Gerando analisador sintático com Happy..."
happy Parser.y

echo "3. Compilando o programa principal com GHC..."
ghc -dynamic --make Main.hs -o compilador

echo ""
echo "=== Compilação concluída com sucesso! ==="
echo "Executável criado: ./compilador"
echo ""
echo "Para testar, execute:"
echo "  ./compilador test.ada"
echo ""