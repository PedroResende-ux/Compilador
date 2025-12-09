-- TAC.hs
-- Three-Address Code generation from AST

module TAC where

import AST
import Data.List (intercalate)

-- State for TAC generation
data TACState = TACState {
    tempCount :: Int,     -- Counter for temporary variables
    labelCount :: Int     -- Counter for labels
} deriving (Show)

-- Initial state
initTACState :: TACState
initTACState = TACState 0 0

-- Generate a new temporary variable
newTemp :: TACState -> (String, TACState)
newTemp state = ("t" ++ show (tempCount state), state { tempCount = tempCount state + 1 })

-- Generate a new label
newLabel :: TACState -> (String, TACState)
newLabel state = ("L" ++ show (labelCount state), state { labelCount = labelCount state + 1 })

-- Generate TAC from Program
generateTAC :: Program -> [TAC]
generateTAC (Program stmts) = 
    let (tac, _) = generateStmtList stmts initTACState
    in tac

-- Generate TAC for a list of statements
generateStmtList :: [Stmt] -> TACState -> ([TAC], TACState)
generateStmtList [] state = ([], state)
generateStmtList (stmt:stmts) state =
    let (tac1, state1) = generateStmt stmt state
        (tac2, state2) = generateStmtList stmts state1
    in (tac1 ++ tac2, state2)

-- Generate TAC for a statement
generateStmt :: Stmt -> TACState -> ([TAC], TACState)
generateStmt EmptyStmt state = ([], state)

generateStmt (Assignment var expr) state =
    let (exprTAC, result, state1) = generateExpr expr state
    in (exprTAC ++ [Assign var result], state1)

generateStmt (IfThenElse cond thenStmt elseStmt) state =
    let (condTAC, condResult, state1) = generateExpr cond state
        (elseLbl, state2) = newLabel state1
        (endLbl, state3) = newLabel state2
        (thenTAC, state4) = generateStmt thenStmt state3
        (elseTAC, state5) = generateStmt elseStmt state4
    in (condTAC ++ 
        [Ifz condResult elseLbl] ++
        thenTAC ++
        [Goto endLbl, Label elseLbl] ++
        elseTAC ++
        [Label endLbl], state5)

generateStmt (IfThen cond thenStmt) state =
    let (condTAC, condResult, state1) = generateExpr cond state
        (endLbl, state2) = newLabel state1
        (thenTAC, state3) = generateStmt thenStmt state2
    in (condTAC ++ 
        [Ifz condResult endLbl] ++
        thenTAC ++
        [Label endLbl], state3)

generateStmt (While cond body) state =
    let (startLbl, state1) = newLabel state
        (endLbl, state2) = newLabel state1
        (condTAC, condResult, state3) = generateExpr cond state2
        (bodyTAC, state4) = generateStmt body state3
    in ([Label startLbl] ++
        condTAC ++
        [Ifz condResult endLbl] ++
        bodyTAC ++
        [Goto startLbl, Label endLbl], state4)

generateStmt (Block stmts) state = generateStmtList stmts state

generateStmt (PutLine expr) state =
    let (exprTAC, result, state1) = generateExpr expr state
        -- For PutLine, we'll treat it as a special operation
        -- We can represent it as a unary operation "Print"
    in (exprTAC ++ [UnOp "_print" result "Print"], state1)

-- Generate TAC for an expression
-- Returns (TAC instructions, result name/value, new state)
generateExpr :: Expr -> TACState -> ([TAC], String, TACState)

-- Literals - return their string representation directly
generateExpr (IntLit n) state = ([], show n, state)
generateExpr (BoolLit True) state = ([], "1", state)
generateExpr (BoolLit False) state = ([], "0", state)
generateExpr (StringLit s) state = ([], "\"" ++ s ++ "\"", state)
generateExpr (Var v) state = ([], v, state)
generateExpr GetLine state = 
    let (temp, state1) = newTemp state
    in ([UnOp temp "_input" "GetLine"], temp, state1)

-- Binary operations
generateExpr (Add e1 e2) state = generateBinOp e1 e2 "Add" state
generateExpr (Sub e1 e2) state = generateBinOp e1 e2 "Sub" state
generateExpr (Mul e1 e2) state = generateBinOp e1 e2 "Mul" state
generateExpr (Div e1 e2) state = generateBinOp e1 e2 "Div" state
generateExpr (Mod e1 e2) state = generateBinOp e1 e2 "Mod" state
generateExpr (And e1 e2) state = generateBinOp e1 e2 "And" state
generateExpr (Or e1 e2) state = generateBinOp e1 e2 "Or" state
generateExpr (Eq e1 e2) state = generateBinOp e1 e2 "Eq" state
generateExpr (Neq e1 e2) state = generateBinOp e1 e2 "Neq" state
generateExpr (Lt e1 e2) state = generateBinOp e1 e2 "Lt" state
generateExpr (Lte e1 e2) state = generateBinOp e1 e2 "Lte" state
generateExpr (Gt e1 e2) state = generateBinOp e1 e2 "Gt" state
generateExpr (Gte e1 e2) state = generateBinOp e1 e2 "Gte" state

-- Unary operations
generateExpr (Neg e) state = generateUnOp e "Neg" state
generateExpr (Not e) state = generateUnOp e "Not" state

-- Helper for binary operations
generateBinOp :: Expr -> Expr -> String -> TACState -> ([TAC], String, TACState)
generateBinOp e1 e2 op state =
    let (tac1, res1, state1) = generateExpr e1 state
        (tac2, res2, state2) = generateExpr e2 state1
        (temp, state3) = newTemp state2
    in (tac1 ++ tac2 ++ [BinOp temp res1 res2 op], temp, state3)

-- Helper for unary operations
generateUnOp :: Expr -> String -> TACState -> ([TAC], String, TACState)
generateUnOp e op state =
    let (tac, res, state1) = generateExpr e state
        (temp, state2) = newTemp state1
    in (tac ++ [UnOp temp res op], temp, state2)

-- Pretty print TAC
prettyPrintTAC :: [TAC] -> String
prettyPrintTAC tacs = intercalate "\n" (map prettyPrintTACInstr tacs)

prettyPrintTACInstr :: TAC -> String
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
