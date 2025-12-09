module Main where

import System.Environment (getArgs)
import System.IO
import System.Exit (exitFailure)
import Data.List (isSuffixOf)
import Lexer
import Parser
import AST
import Semantic
import TAC
import MIPS

printTree :: Program -> String
printTree (Program decls stmts) = 
  "Program\n" ++ 
  (if null decls then "" else "  Declarations:\n" ++ concatMap (printDecl 2) decls) ++
  "  Statements:\n" ++ concatMap (printStmt 2) stmts

printDecl :: Int -> Decl -> String
printDecl level (VarDecl name typ) =
  let indent = replicate (level * 2) ' '
      branch = "├─ "
  in indent ++ branch ++ "VarDecl: " ++ name ++ " : " ++ show typ ++ "\n"

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
  
  -- Determine output filename
  let outputFile = case args of
        (filename:_) -> 
          -- Remove the extension and add .asm
          let withoutExt = if ".ada" `isSuffixOf` filename
                          then take (length filename - 4) filename
                          else filename
          in withoutExt ++ ".asm"
        [] -> "output.asm"
  
  -- Análise léxica
  let tokens = alexScanTokens input
  
  -- Análise sintática
  let ast = parse tokens
  
  -- Análise semântica
  let semanticResult = analyzeProgram ast
  
  -- Print AST
  putStrLn "=== ABSTRACT SYNTAX TREE ==="
  putStrLn $ printTree ast
  putStrLn ""
  
  -- Print semantic analysis results
  putStrLn "=== SEMANTIC ANALYSIS ==="
  if null (errors semanticResult)
    then putStrLn "✓ No semantic errors found"
    else do
      putStrLn "✗ Semantic errors found:"
      mapM_ (putStrLn . ("  " ++)) (errors semanticResult)
      exitFailure
  
  if null (warnings semanticResult)
    then return ()
    else do
      putStrLn "Warnings:"
      mapM_ (putStrLn . ("  " ++)) (warnings semanticResult)
  
  putStrLn ""
  putStrLn "=== SYMBOL TABLE ==="
  print (symbolTable semanticResult)
  putStrLn ""
  
  -- Gerar Three-Address Code (TAC)
  let tac = generateTAC ast
  
  putStrLn "=== THREE-ADDRESS CODE ==="
  putStrLn $ prettyPrintTAC tac
  putStrLn ""
  
  -- Gerar código MIPS
  let mipsCode = generateMIPS tac
  
  putStrLn "=== MIPS ASSEMBLY CODE ==="
  putStrLn mipsCode
  putStrLn ""
  
  -- Write MIPS code to file
  writeFile outputFile mipsCode
  putStrLn $ "MIPS code written to: " ++ outputFile
