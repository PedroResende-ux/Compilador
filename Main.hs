-- Main.hs
-- Programa principal do compilador Ada

module Main where

import System.Environment (getArgs)
import System.IO
import Lexer
import Parser
import AST

-- Função para imprimir a AST de forma legível
prettyPrint :: Program -> String
prettyPrint (Program stmts) = 
  "Program:\n" ++ concatMap (\s -> "  " ++ ppStmt s 1 ++ "\n") stmts

ppStmt :: Stmt -> Int -> String
ppStmt stmt indent = case stmt of
  Assignment var expr -> 
    replicate (indent*2) ' ' ++ var ++ " := " ++ ppExpr expr
  
  IfThenElse cond thenStmt elseStmt ->
    replicate (indent*2) ' ' ++ "if " ++ ppExpr cond ++ " then\n" ++
    ppStmt thenStmt (indent+1) ++ "\n" ++
    replicate (indent*2) ' ' ++ "else\n" ++
    ppStmt elseStmt (indent+1)
  
  IfThen cond thenStmt ->
    replicate (indent*2) ' ' ++ "if " ++ ppExpr cond ++ " then\n" ++
    ppStmt thenStmt (indent+1)
  
  While cond body ->
    replicate (indent*2) ' ' ++ "while " ++ ppExpr cond ++ " loop\n" ++
    ppStmt body (indent+1) ++ "\n" ++
    replicate (indent*2) ' ' ++ "end loop"
  
  Block stmts ->
    replicate (indent*2) ' ' ++ "begin\n" ++
    concatMap (\s -> ppStmt s (indent+1) ++ "\n") stmts ++
    replicate (indent*2) ' ' ++ "end"
  
  PutLine expr ->
    replicate (indent*2) ' ' ++ "Put_Line(" ++ ppExpr expr ++ ")"
  
  EmptyStmt -> replicate (indent*2) ' ' ++ "<empty>"

ppExpr :: Expr -> String
ppExpr expr = case expr of
  IntLit n -> show n
  BoolLit b -> show b
  StringLit s -> "\"" ++ s ++ "\""
  Var v -> v
  Add e1 e2 -> "(" ++ ppExpr e1 ++ " + " ++ ppExpr e2 ++ ")"
  Sub e1 e2 -> "(" ++ ppExpr e1 ++ " - " ++ ppExpr e2 ++ ")"
  Mul e1 e2 -> "(" ++ ppExpr e1 ++ " * " ++ ppExpr e2 ++ ")"
  Div e1 e2 -> "(" ++ ppExpr e1 ++ " / " ++ ppExpr e2 ++ ")"
  Mod e1 e2 -> "(" ++ ppExpr e1 ++ " mod " ++ ppExpr e2 ++ ")"
  Neg e -> "(-" ++ ppExpr e ++ ")"
  And e1 e2 -> "(" ++ ppExpr e1 ++ " and " ++ ppExpr e2 ++ ")"
  Or e1 e2 -> "(" ++ ppExpr e1 ++ " or " ++ ppExpr e2 ++ ")"
  Not e -> "(not " ++ ppExpr e ++ ")"
  Eq e1 e2 -> "(" ++ ppExpr e1 ++ " = " ++ ppExpr e2 ++ ")"
  Neq e1 e2 -> "(" ++ ppExpr e1 ++ " /= " ++ ppExpr e2 ++ ")"
  Lt e1 e2 -> "(" ++ ppExpr e1 ++ " < " ++ ppExpr e2 ++ ")"
  Lte e1 e2 -> "(" ++ ppExpr e1 ++ " <= " ++ ppExpr e2 ++ ")"
  Gt e1 e2 -> "(" ++ ppExpr e1 ++ " > " ++ ppExpr e2 ++ ")"
  Gte e1 e2 -> "(" ++ ppExpr e1 ++ " >= " ++ ppExpr e2 ++ ")"
  GetLine -> "Get_Line"

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> getContents  -- lê da entrada padrão
    (filename:_) -> readFile filename  -- lê do ficheiro
  
  -- Análise léxica
  let tokens = alexScanTokens input
  
  putStrLn "=== TOKENS ==="
  mapM_ print tokens
  putStrLn ""
  
  -- Análise sintática
  let ast = parse tokens
  
  putStrLn "=== ABSTRACT SYNTAX TREE ==="
  putStrLn $ prettyPrint ast
  putStrLn ""
  
  putStrLn "=== HASKELL AST REPRESENTATION ==="
  print ast