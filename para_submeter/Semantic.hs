-- Semantic.hs
-- Análise semântica para o compilador do subconjunto de Ada
--
-- Realiza verificações semânticas:
--   - Variáveis devem ser declaradas antes do uso
--   - Sem redeclarações no mesmo escopo
--   - Escopos aninhados corretamente (blocos)
--
-- Retorna SemanticResult com:
--   - errors: Lista de erros semânticos encontrados
--   - warnings: Lista de avisos (atualmente não utilizado)
--   - symbolTable: Estado final da tabela de símbolos

module Semantic where

import AST
import qualified Data.Map as Map

-- Resultado da análise semântica
data SemanticResult = SemanticResult
  { errors :: [String]       -- Erros encontrados
  , warnings :: [String]     -- Avisos (preparado para futuro)
  , symbolTable :: SymbolTable  -- Tabela de símbolos final
  }
  deriving (Show)

-- Realizar análise semântica num programa
analyzeProgram :: Program -> SemanticResult
analyzeProgram (Program decls stmts) =
  let initialST = emptySymbolTable
      -- Processar declarações primeiro
      (declErrors, stAfterDecls) = processDeclarations decls initialST
      -- Verificar comandos para variáveis não declaradas
      stmtErrors = checkStatements stmts stAfterDecls
  in SemanticResult 
     { errors = declErrors ++ stmtErrors
     , warnings = []
     , symbolTable = stAfterDecls
     }

-- Processar todas as declarações e verificar redeclarações
-- Processo de inserção com verificação semântica
processDeclarations :: [Decl] -> SymbolTable -> ([String], SymbolTable)
processDeclarations [] st = ([], st)
processDeclarations (VarDecl name typ : rest) st =
  case insertSymbol name typ st of
    Nothing -> 
      let (errs, finalST) = processDeclarations rest st
          errorMsg = "Error: Variable '" ++ name ++ "' already declared in this scope"
      in (errorMsg : errs, finalST)
    Just newST -> processDeclarations rest newST

-- Verificar comandos para variáveis não declaradas
checkStatements :: [Stmt] -> SymbolTable -> [String]
checkStatements stmts st = concatMap (checkStmt st) stmts

-- Verificar um comando (recursão sobre statements)
checkStmt :: SymbolTable -> Stmt -> [String]
checkStmt st stmt = case stmt of
  Assignment var expr ->
    let varErrors = if isDeclared var st
                    then []
                    else ["Error: Variable '" ++ var ++ "' used but not declared"]
        exprErrors = checkExpr st expr
    in varErrors ++ exprErrors
  
  IfThenElse cond thenStmt elseStmt ->
    checkExpr st cond ++ checkStmt st thenStmt ++ checkStmt st elseStmt
  
  IfThen cond thenStmt ->
    checkExpr st cond ++ checkStmt st thenStmt
  
  While cond body ->
    checkExpr st cond ++ checkStmt st body
  
  Block stmts ->
    -- Entrar num novo âmbito para o bloco
    let stInBlock = enterScope st
        blockErrors = checkStatements stmts stInBlock
    in blockErrors
  
  PutLine expr ->
    checkExpr st expr
  
  EmptyStmt -> []

-- Verificar expressões para variáveis não declaradas (recursão sobre expressões)
checkExpr :: SymbolTable -> Expr -> [String]
checkExpr st expr = case expr of
  Var v -> 
    if isDeclared v st
    then []
    else ["Error: Variable '" ++ v ++ "' used but not declared"]
  
  -- Operações binárias: verificar ambos os operandos
  Add e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Sub e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Mul e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Div e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Mod e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Neg e -> checkExpr st e
  
  And e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Or e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Not e -> checkExpr st e
  
  Eq e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Neq e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Lt e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Lte e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Gt e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  Gte e1 e2 -> checkExpr st e1 ++ checkExpr st e2
  
  -- Literals don't need checking
  IntLit _ -> []
  BoolLit _ -> []
  StringLit _ -> []
  GetLine -> []
