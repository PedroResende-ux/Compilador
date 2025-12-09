-- Semantic.hs
-- Semantic analysis for the Ada subset compiler

module Semantic where

import AST
import qualified Data.Map as Map

-- Result type for semantic analysis
data SemanticResult = SemanticResult
  { errors :: [String]
  , warnings :: [String]
  , symbolTable :: SymbolTable
  }
  deriving (Show)

-- Perform semantic analysis on a program
analyzeProgram :: Program -> SemanticResult
analyzeProgram (Program decls stmts) =
  let initialST = emptySymbolTable
      -- Process declarations first
      (declErrors, stAfterDecls) = processDeclarations decls initialST
      -- Check statements for undeclared variables
      stmtErrors = checkStatements stmts stAfterDecls
  in SemanticResult 
     { errors = declErrors ++ stmtErrors
     , warnings = []
     , symbolTable = stAfterDecls
     }

-- Process all declarations and check for redeclarations
processDeclarations :: [Decl] -> SymbolTable -> ([String], SymbolTable)
processDeclarations [] st = ([], st)
processDeclarations (VarDecl name typ : rest) st =
  case insertSymbol name typ st of
    Nothing -> 
      let (errs, finalST) = processDeclarations rest st
          errorMsg = "Error: Variable '" ++ name ++ "' already declared in this scope"
      in (errorMsg : errs, finalST)
    Just newST -> processDeclarations rest newST

-- Check statements for undeclared variables
checkStatements :: [Stmt] -> SymbolTable -> [String]
checkStatements stmts st = concatMap (checkStmt st) stmts

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
    -- Enter new scope for block
    let stInBlock = enterScope st
        blockErrors = checkStatements stmts stInBlock
    in blockErrors
  
  PutLine expr ->
    checkExpr st expr
  
  EmptyStmt -> []

-- Check expressions for undeclared variables
checkExpr :: SymbolTable -> Expr -> [String]
checkExpr st expr = case expr of
  Var v -> 
    if isDeclared v st
    then []
    else ["Error: Variable '" ++ v ++ "' used but not declared"]
  
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
