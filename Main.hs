module Main where

import System.Environment (getArgs)
import System.IO
import Lexer
import Parser
import AST

printTree :: Program -> String
printTree (Program stmts) = 
  "Program\n" ++ concatMap (printStmt 1) stmts

printStmt :: Int -> Stmt -> String
printStmt level stmt = 
  let indent = replicate (level * 2) ' '
      branch = "├─ "
  in case stmt of
    Assignment var expr -> 
      indent ++ branch ++ "Assignment\n" ++
      indent ++ "  ├─ Variable: " ++ var ++ "\n" ++
      indent ++ "  └─ " ++ printExpr (level + 1) expr
    
    IfThenElse cond thenStmt elseStmt ->
      indent ++ branch ++ "IfThenElse\n" ++
      indent ++ "  ├─ Condition: " ++ printExpr (level + 1) cond ++
      indent ++ "  ├─ Then:\n" ++ printStmt (level + 2) thenStmt ++
      indent ++ "  └─ Else:\n" ++ printStmt (level + 2) elseStmt
    
    IfThen cond thenStmt ->
      indent ++ branch ++ "IfThen\n" ++
      indent ++ "  ├─ Condition: " ++ printExpr (level + 1) cond ++
      indent ++ "  └─ Then:\n" ++ printStmt (level + 2) thenStmt
    
    While cond body ->
      indent ++ branch ++ "While\n" ++
      indent ++ "  ├─ Condition: " ++ printExpr (level + 1) cond ++
      indent ++ "  └─ Body:\n" ++ printStmt (level + 2) body
    
    Block stmts ->
      indent ++ branch ++ "Block\n" ++
      concatMap (printStmt (level + 1)) stmts
    
    PutLine expr ->
      indent ++ branch ++ "PutLine\n" ++
      indent ++ "  └─ " ++ printExpr (level + 1) expr
    
    EmptyStmt -> 
      indent ++ branch ++ "EmptyStmt\n"

printExpr :: Int -> Expr -> String
printExpr level expr = 
  let indent = replicate (level * 2) ' '
  in case expr of
    IntLit n -> "IntLit: " ++ show n ++ "\n"
    BoolLit b -> "BoolLit: " ++ show b ++ "\n"
    StringLit s -> "StringLit: \"" ++ s ++ "\"\n"
    Var v -> "Var: " ++ v ++ "\n"
    
    Add e1 e2 -> 
      "Add\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Sub e1 e2 -> 
      "Sub\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Mul e1 e2 -> 
      "Mul\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Div e1 e2 -> 
      "Div\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Mod e1 e2 -> 
      "Mod\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Neg e -> 
      "Neg\n" ++
      indent ++ "  └─ " ++ printExpr (level + 1) e
    
    And e1 e2 -> 
      "And\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Or e1 e2 -> 
      "Or\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Not e -> 
      "Not\n" ++
      indent ++ "  └─ " ++ printExpr (level + 1) e
    
    Eq e1 e2 -> 
      "Eq\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Neq e1 e2 -> 
      "Neq\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Lt e1 e2 -> 
      "Lt\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Lte e1 e2 -> 
      "Lte\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Gt e1 e2 -> 
      "Gt\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    Gte e1 e2 -> 
      "Gte\n" ++
      indent ++ "  ├─ " ++ printExpr (level + 1) e1 ++
      indent ++ "  └─ " ++ printExpr (level + 1) e2
    
    GetLine -> "GetLine\n"

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    [] -> getContents  
    (filename:_) -> readFile filename 
  
  -- Análise léxica
  let tokens = alexScanTokens input
  
  -- Análise sintática
  let ast = parse tokens
  
  putStrLn $ printTree ast


generateTACExpr :: Expr -> Int -> ([TAC], String, Int)
generateTACExpr (IntLit n) tempCount =
    ([], show n, tempCount)  -- Literals directly return themselves

generateTACExpr (Add e1 e2) tempCount =
    let
        (tac1, res1, tempCount1) = generateTACExpr e1 tempCount
        (tac2, res2, tempCount2) = generateTACExpr e2 tempCount1
        tempVar = "t" ++ show tempCount2
        currTAC = BinOp tempVar res1 res2 "Add"
    in
        (tac1 ++ tac2 ++ [currTAC], tempVar, tempCount2 + 1)
