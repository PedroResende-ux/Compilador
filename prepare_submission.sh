#!/bin/bash

# Script para preparar submissão do Trabalho Prático 2
# Compiladores - DCC FCUP

set -e  # Parar em caso de erro

echo "🎓 Preparando submissão do Trabalho Prático 2..."
echo ""

# Configuração
GRUPO_NUM="XX"  # ⚠️ ALTERAR PARA O NÚMERO DO SEU GRUPO
ZIP_NAME="COMP_TP2_GRUPO_${GRUPO_NUM}.zip"
SUBMISSION_DIR="ferramentas"

# 1. Limpar lixo
echo "🧹 1. Limpando ficheiros de lixo..."
cd para_submeter
make clean 2>/dev/null || true
cd ..

# 2. Limpar pasta ferramentas
echo "🗑️  2. Limpando pasta ferramentas anterior..."
rm -rf ${SUBMISSION_DIR}/*
mkdir -p ${SUBMISSION_DIR}

# 3. Copiar código fonte
echo "📁 3. Copiando código fonte..."
cp para_submeter/Lexer.x para_submeter/Parser.y para_submeter/AST.hs para_submeter/Token.hs \
   para_submeter/Semantic.hs para_submeter/TAC.hs para_submeter/MIPS.hs para_submeter/Main.hs \
   para_submeter/Makefile para_submeter/build.sh para_submeter/run_tests.sh \
   ${SUBMISSION_DIR}/

# 4. Copiar testes
echo "🧪 4. Copiando testes..."
mkdir -p ${SUBMISSION_DIR}/testes
cp testes/*.ada ${SUBMISSION_DIR}/testes/

# 5. Criar README.pdf
echo "📄 5. Criando README.pdf..."
if command -v pandoc &> /dev/null; then
    pandoc documentacao/README.md -o ${SUBMISSION_DIR}/README.pdf
    echo "   ✅ README.pdf criado com pandoc"
else
    echo "   ⚠️  pandoc não encontrado. Converta documentacao/README.md manualmente para PDF"
    echo "   Coloque o PDF em ${SUBMISSION_DIR}/README.pdf"
fi

# 6. Remover ficheiros desnecessários
echo "🧹 6. Removendo ficheiros desnecessários..."
find ${SUBMISSION_DIR} -name ".DS_Store" -delete 2>/dev/null || true
find ${SUBMISSION_DIR} -name "*.hi" -delete 2>/dev/null || true
find ${SUBMISSION_DIR} -name "*.o" -delete 2>/dev/null || true
find ${SUBMISSION_DIR} -name "Lexer.hs" -delete 2>/dev/null || true
find ${SUBMISSION_DIR} -name "Parser.hs" -delete 2>/dev/null || true
find ${SUBMISSION_DIR} -name "compilador" -delete 2>/dev/null || true
find ${SUBMISSION_DIR} -name "*.asm" -delete 2>/dev/null || true

# 7. Verificar estrutura
echo ""
echo "📋 7. Verificando estrutura do submission/..."
echo ""
find ${SUBMISSION_DIR} -type f | sort

# 8. Criar .zip
echo ""
echo "📦 8. Criando ficheiro .zip..."
cd ${SUBMISSION_DIR}
zip -r ../${ZIP_NAME} . > /dev/null
cd ..

# 9. Mostrar resultado
echo ""
echo "✅ SUBMISSÃO PRONTA!"
echo ""
echo "📦 Ficheiro criado: ${ZIP_NAME}"
echo "📊 Tamanho: $(du -h ${ZIP_NAME} | cut -f1)"
echo ""

# 10. Listar conteúdo do .zip
echo "📋 Conteúdo do .zip:"
unzip -l ${ZIP_NAME} | head -30

echo ""
echo "⚠️  IMPORTANTE:"
echo "   1. Verifique se README.pdf foi criado em ${SUBMISSION_DIR}/"
echo "   2. Se não, converta docs/README.md manualmente"
echo "   3. Altere GRUPO_NUM no topo deste script para o número correto"
echo "   4. Teste o .zip extraindo e compilando: unzip ${ZIP_NAME} && make"
echo ""
echo "✅ Pronto para submeter no Moodle!"
