-- TAC.hs
-- Geração de código de três endereços a partir da AST
--
-- Gera representação intermédia com:
--   - Operações binárias: dest := src1 op src2
--   - Operações unárias: dest := op src
--   - Atribuições: dest := src
--   - Saltos condicionais: ifz var label
--   - Saltos incondicionais: goto label
--   - Rótulos: label:
--
-- Funções principais:
--   - generateCode: Converte Program para lista de instruções TAC
--   - transExpr: Traduz expressões para TAC
--   - transStm: Traduz comandos para TAC
--   - newTemp: Gera nomes únicos de variáveis temporárias
--   - newLabel: Gera nomes únicos de rótulos
--
-- Notas de implementação:
--   - Usa threading explícito de estado (TACState) para manter contadores
--   - Temporários: t0, t1, t2, ...
--   - Rótulos: L0, L1, L2, ...

module TAC where

import AST
import Data.List (intercalate)

-- Estado para geração de TAC
-- Mantém contadores para variáveis temporárias e rótulos únicos
data TACState = TACState {
    tempCount :: Int,     -- Contador para variáveis temporárias
    labelCount :: Int     -- Contador para labels
} deriving (Show)

-- Estado inicial
initTACState :: TACState
initTACState = TACState 0 0

-- Gerar nova variável temporária única
newTemp :: TACState -> (String, TACState)
newTemp state = ("t" ++ show (tempCount state), state { tempCount = tempCount state + 1 })

-- Gerar novo rótulo único
newLabel :: TACState -> (String, TACState)
newLabel state = ("L" ++ show (labelCount state), state { labelCount = labelCount state + 1 })

-- Gerar código intermédio de um Programa
-- Nota: Declarações são tratadas na análise semântica e guardadas na tabela de símbolos.
-- Geração de código foca apenas em comandos executáveis, pois declarações de variáveis não
-- geram código runtime - são usadas para verificação semântica e informação de tipos.
generateCode :: Program -> [Instr]
generateCode (Program _ stmts) = 
    let (instrs, _) = transStmList stmts initTACState
    in instrs

-- Traduzir lista de comandos para instruções
transStmList :: [Stmt] -> TACState -> ([Instr], TACState)
transStmList [] state = ([], state)
transStmList (stmt:stmts) state =
    let (instrs1, state1) = transStm stmt state
        (instrs2, state2) = transStmList stmts state1
    in (instrs1 ++ instrs2, state2)

-- Traduzir comando para instruções TAC
transStm :: Stmt -> TACState -> ([Instr], TACState)
transStm EmptyStmt state = ([], state)

transStm (Assignment var expr) state =
    let (exprInstrs, result, state1) = transExpr expr state
    in (exprInstrs ++ [Assign var result], state1)

transStm (IfThenElse cond thenStmt elseStmt) state =
    let (condInstrs, condResult, state1) = transExpr cond state
        (elseLbl, state2) = newLabel state1
        (endLbl, state3) = newLabel state2
        (thenInstrs, state4) = transStm thenStmt state3
        (elseInstrs, state5) = transStm elseStmt state4
    in (condInstrs ++ 
        [Ifz condResult elseLbl] ++
        thenInstrs ++
        [Goto endLbl, Label elseLbl] ++
        elseInstrs ++
        [Label endLbl], state5)

transStm (IfThen cond thenStmt) state =
    let (condInstrs, condResult, state1) = transExpr cond state
        (endLbl, state2) = newLabel state1
        (thenInstrs, state3) = transStm thenStmt state2
    in (condInstrs ++ 
        [Ifz condResult endLbl] ++
        thenInstrs ++
        [Label endLbl], state3)

transStm (While cond body) state =
    let (startLbl, state1) = newLabel state
        (endLbl, state2) = newLabel state1
        (condInstrs, condResult, state3) = transExpr cond state2
        (bodyInstrs, state4) = transStm body state3
    in ([Label startLbl] ++
        condInstrs ++
        [Ifz condResult endLbl] ++
        bodyInstrs ++
        [Goto startLbl, Label endLbl], state4)

transStm (Block stmts) state = transStmList stmts state

transStm (PutLine expr) state =
    let (exprInstrs, result, state1) = transExpr expr state
        -- Para PutLine, tratamos como operação especial
        -- Representamos como operação unária "Print"
    in (exprInstrs ++ [UnOp "_print" result "Print"], state1)

-- Traduzir expressão para instruções
-- Esquema de transformação de expressões para TAC
-- Retorna (instruções, nome/valor do resultado, novo estado)
transExpr :: Expr -> TACState -> ([Instr], String, TACState)

-- Literais - retornam representação em string diretamente
transExpr (IntLit n) state = ([], show n, state)
transExpr (BoolLit True) state = ([], "1", state)
transExpr (BoolLit False) state = ([], "0", state)
transExpr (StringLit s) state = ([], "\"" ++ s ++ "\"", state)
transExpr (Var v) state = ([], v, state)
transExpr GetLine state = 
    let (temp, state1) = newTemp state
    in ([UnOp temp "_input" "GetLine"], temp, state1)

-- Operações binárias
-- Caso binário: e1 op e2
transExpr (Add e1 e2) state = transBinOp e1 e2 "Add" state
transExpr (Sub e1 e2) state = transBinOp e1 e2 "Sub" state
transExpr (Mul e1 e2) state = transBinOp e1 e2 "Mul" state
transExpr (Div e1 e2) state = transBinOp e1 e2 "Div" state
transExpr (Mod e1 e2) state = transBinOp e1 e2 "Mod" state
transExpr (And e1 e2) state = transBinOp e1 e2 "And" state
transExpr (Or e1 e2) state = transBinOp e1 e2 "Or" state
transExpr (Eq e1 e2) state = transBinOp e1 e2 "Eq" state
transExpr (Neq e1 e2) state = transBinOp e1 e2 "Neq" state
transExpr (Lt e1 e2) state = transBinOp e1 e2 "Lt" state
transExpr (Lte e1 e2) state = transBinOp e1 e2 "Lte" state
transExpr (Gt e1 e2) state = transBinOp e1 e2 "Gt" state
transExpr (Gte e1 e2) state = transBinOp e1 e2 "Gte" state

-- Operações unárias (extensão necessária para Ada: - e not)
transExpr (Neg e) state = transUnOp e "Neg" state
transExpr (Not e) state = transUnOp e "Not" state

-- Função auxiliar para operações binárias
-- Derivado do algoritmo de transformação de expressões
transBinOp :: Expr -> Expr -> String -> TACState -> ([Instr], String, TACState)
transBinOp e1 e2 op state =
    let (instrs1, res1, state1) = transExpr e1 state
        (instrs2, res2, state2) = transExpr e2 state1
        (temp, state3) = newTemp state2
    in (instrs1 ++ instrs2 ++ [BinOp temp res1 res2 op], temp, state3)

-- Função auxiliar para operações unárias
transUnOp :: Expr -> String -> TACState -> ([Instr], String, TACState)
transUnOp e op state =
    let (instrs, res, state1) = transExpr e state
        (temp, state2) = newTemp state1
    in (instrs ++ [UnOp temp res op], temp, state2)

-- Pretty print Instr (código intermédio)
prettyPrintTAC :: [Instr] -> String
prettyPrintTAC instrs = intercalate "\n" (map prettyPrintTACInstr instrs)

prettyPrintTACInstr :: Instr -> String
prettyPrintTACInstr (Assign dest src) = "  " ++ dest ++ " = " ++ src
prettyPrintTACInstr (BinOp dest src1 src2 op) = 
    "  " ++ dest ++ " = " ++ src1 ++ " " ++ opSymbol op ++ " " ++ src2
prettyPrintTACInstr (UnOp dest src op) = 
    "  " ++ dest ++ " = " ++ opSymbol op ++ " " ++ src
prettyPrintTACInstr (Goto lbl) = "  goto " ++ lbl
prettyPrintTACInstr (Ifz cond lbl) = "  ifz " ++ cond ++ " goto " ++ lbl
prettyPrintTACInstr (Label lbl) = lbl ++ ":"

-- Convert operation name to symbol for pretty printing
opSymbol :: String -> String
opSymbol "Add" = "+"
opSymbol "Sub" = "-"
opSymbol "Mul" = "*"
opSymbol "Div" = "/"
opSymbol "Mod" = "mod"
opSymbol "And" = "and"
opSymbol "Or" = "or"
opSymbol "Not" = "not"
opSymbol "Neg" = "-"
opSymbol "Eq" = "="
opSymbol "Neq" = "/="
opSymbol "Lt" = "<"
opSymbol "Lte" = "<="
opSymbol "Gt" = ">"
opSymbol "Gte" = ">="
opSymbol "Print" = "print"
opSymbol "GetLine" = "getline"
opSymbol op = op
