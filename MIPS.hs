-- MIPS.hs
-- MIPS assembly code generation from TAC

module MIPS where

import AST (TAC(..))
import Data.List (intercalate)
import qualified Data.Map as Map

-- Variable to register/memory mapping
data MIPSState = MIPSState {
    varMap :: Map.Map String String,  -- Maps variables to registers or memory locations
    nextStackOffset :: Int,            -- Next available stack offset
    stringLiterals :: [(String, String)], -- String literals and their labels
    nextVarReg :: Int                 -- Next available variable register
} deriving (Show)

-- Initial MIPS state
initMIPSState :: MIPSState
initMIPSState = MIPSState Map.empty 0 [] 0

-- Generate MIPS code from TAC
generateMIPS :: [TAC] -> String
generateMIPS tacs = 
    let (dataSection, state) = extractStrings tacs initMIPSState
        state' = allocateVars tacs state
        textSection = generateMIPSText tacs state'
    in mipsPreamble ++ dataSection ++ "\n.text\n.globl main\nmain:\n" ++ textSection ++ mipsPostamble

-- Allocate registers for all variables used in TAC
allocateVars :: [TAC] -> MIPSState -> MIPSState
allocateVars tacs state = foldl allocateVar state (collectVars tacs)
  where
    collectVars :: [TAC] -> [String]
    collectVars = nub . concatMap getVarsFromTAC
    
    getVarsFromTAC (Assign dest src) = [dest | not (isTemp dest)] ++ [src | isVar src]
    getVarsFromTAC (BinOp dest src1 src2 _) = 
        [dest | not (isTemp dest)] ++ [src1 | isVar src1] ++ [src2 | isVar src2]
    getVarsFromTAC (UnOp dest src _) = [dest | not (isTemp dest)] ++ [src | isVar src]
    getVarsFromTAC _ = []
    
    isVar v = not (isImmediate v) && not (isStringLiteral v) && not (isTemp v) && v /= "_print" && v /= "_input"
    isTemp v = "t" `isPrefixOf` v
    
    allocateVar st var = 
        if Map.member var (varMap st)
        then st
        else let reg = "$s" ++ show (nextVarReg st)
                 newMap = Map.insert var reg (varMap st)
             in st { varMap = newMap, nextVarReg = nextVarReg st + 1 }
    
    nub :: Eq a => [a] -> [a]
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)
    
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Extract string literals for .data section
extractStrings :: [TAC] -> MIPSState -> (String, MIPSState)
extractStrings tacs state = 
    let strings = collectStrings tacs [] 0
        dataSection = if null strings 
                      then "" 
                      else ".data\n" ++ concatMap formatString strings
    in (dataSection, state { stringLiterals = strings })
  where
    collectStrings [] acc _ = reverse acc
    collectStrings (tac:rest) acc n = 
        case getStringFromTAC tac of
            Just s -> collectStrings rest ((s, "str" ++ show n) : acc) (n + 1)
            Nothing -> collectStrings rest acc n
    
    formatString (str, lbl) = lbl ++ ": .asciiz " ++ str ++ "\n"
    
    getStringFromTAC (Assign _ src) = if isStringLiteral src then Just src else Nothing
    getStringFromTAC (BinOp _ src1 src2 _) = 
        case (isStringLiteral src1, isStringLiteral src2) of
            (True, _) -> Just src1
            (_, True) -> Just src2
            _ -> Nothing
    getStringFromTAC (UnOp _ src _) = if isStringLiteral src then Just src else Nothing
    getStringFromTAC _ = Nothing
    
    isStringLiteral s = not (null s) && head s == '"' && last s == '"'

-- Generate MIPS code for TAC instructions
generateMIPSText :: [TAC] -> MIPSState -> String
generateMIPSText tacs state = intercalate "\n" (map (generateMIPSInstr state) tacs)

-- Generate MIPS instruction from a single TAC instruction
generateMIPSInstr :: MIPSState -> TAC -> String

generateMIPSInstr state (Label lbl) = lbl ++ ":"

generateMIPSInstr state (Goto lbl) = "  j " ++ lbl

generateMIPSInstr state (Ifz cond lbl) = 
    let reg = getRegister cond state
    in "  beqz " ++ reg ++ ", " ++ lbl

generateMIPSInstr state (Assign dest src) =
    let destReg = getRegister dest state
        srcReg = getRegister src state
    in if isImmediate src
       then "  li " ++ destReg ++ ", " ++ src
       else "  move " ++ destReg ++ ", " ++ srcReg

generateMIPSInstr state (BinOp dest src1 src2 op) =
    let destReg = getRegister dest state
    in case op of
        "Add" -> 
            if isImmediate src2
            then "  addi " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ src2
            else "  add " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Sub" -> 
            if isImmediate src2
            then let negVal = show (-(read src2 :: Int))
                 in "  addi " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ negVal
            else "  sub " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Mul" -> 
            if isImmediate src1 && isImmediate src2
            then let result = show ((read src1 :: Int) * (read src2 :: Int))
                 in "  li " ++ destReg ++ ", " ++ result
            else if isImmediate src2
            then "  mul " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ src2
            else if isImmediate src1
            then "  mul " ++ destReg ++ ", " ++ getRegOrImm src2 state ++ ", " ++ src1
            else "  mul " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Div" -> 
            let src1Reg = getRegOrImm src1 state
                src2Reg = getRegOrImm src2 state
            in "  div " ++ src1Reg ++ ", " ++ src2Reg ++ "\n" ++
               "  mflo " ++ destReg
        "Mod" -> 
            let src1Reg = getRegOrImm src1 state
                src2Reg = getRegOrImm src2 state
            in "  div " ++ src1Reg ++ ", " ++ src2Reg ++ "\n" ++
               "  mfhi " ++ destReg
        "And" -> "  and " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Or" -> "  or " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Eq" -> 
            if isImmediate src2
            then "  seq " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ src2
            else "  seq " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Neq" -> 
            if isImmediate src2
            then "  sne " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ src2
            else "  sne " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Lt" -> 
            if isImmediate src2
            then "  slti " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ src2
            else "  slt " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Lte" -> 
            if isImmediate src2
            then let val = show ((read src2 :: Int) + 1)
                 in "  slti " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ val
            else "  sle " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Gt" -> 
            if isImmediate src2
            then let val = show ((read src2 :: Int) + 1)
                 in "  # " ++ destReg ++ " = " ++ getRegOrImm src1 state ++ " > " ++ src2 ++ "\n" ++
                    "  slti " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ val ++ "\n" ++
                    "  xori " ++ destReg ++ ", " ++ destReg ++ ", 1"
            else "  sgt " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        "Gte" -> 
            if isImmediate src2
            then "  # " ++ destReg ++ " = " ++ getRegOrImm src1 state ++ " >= " ++ src2 ++ "\n" ++
                 "  slti " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ src2 ++ "\n" ++
                 "  xori " ++ destReg ++ ", " ++ destReg ++ ", 1"
            else "  sge " ++ destReg ++ ", " ++ getRegOrImm src1 state ++ ", " ++ getRegOrImm src2 state
        _ -> "  # Unknown operation: " ++ op

generateMIPSInstr state (UnOp dest src op) =
    let destReg = getRegister dest state
        srcReg = getRegister src state
    in case op of
        "Neg" -> "  neg " ++ destReg ++ ", " ++ srcReg
        "Not" -> 
            "  seq " ++ destReg ++ ", " ++ srcReg ++ ", $zero"
        "Print" -> 
            if isStringLiteral src
            then let strLabel = getStringLabel src state
                 in "  la $a0, " ++ strLabel ++ "\n" ++
                    "  li $v0, 4\n" ++
                    "  syscall"
            else "  move $a0, " ++ srcReg ++ "\n" ++
                 "  li $v0, 1\n" ++
                 "  syscall\n" ++
                 "  la $a0, newline\n" ++
                 "  li $v0, 4\n" ++
                 "  syscall"
        "GetLine" -> 
            "  li $v0, 5\n" ++
            "  syscall\n" ++
            "  move " ++ destReg ++ ", $v0"
        _ -> "  # Unknown unary operation: " ++ op

-- Get register or immediate value
getRegOrImm :: String -> MIPSState -> String
getRegOrImm val state = 
    if isImmediate val
    then val
    else getRegister val state

-- Check if a value is an immediate (numeric constant)
isImmediate :: String -> Bool
isImmediate s = case reads s :: [(Int, String)] of
    [(_, "")] -> True
    _ -> False

-- Check if a value is a string literal
isStringLiteral :: String -> Bool
isStringLiteral s = not (null s) && head s == '"' && last s == '"'

-- Get string label for a string literal
getStringLabel :: String -> MIPSState -> String
getStringLabel str state = 
    case lookup str (stringLiterals state) of
        Just lbl -> lbl
        Nothing -> "str_unknown"  -- Fallback

-- Get MIPS register or memory location for a variable/temporary
getRegister :: String -> MIPSState -> String
getRegister name state
    | name == "_print" = "$a0"  -- Special case for print
    | name == "_input" = "$v0"  -- Special case for input
    | "t" `isPrefixOf` name = 
        let num = read (drop 1 name) :: Int
        in if num < 10 then "$t" ++ show num else "$t9"  -- Use t9 for overflow
    | otherwise = 
        case Map.lookup name (varMap state) of
            Just reg -> reg
            Nothing -> "$s0"  -- Fallback to s0
  where
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- MIPS preamble with syscall setup
mipsPreamble :: String
mipsPreamble = unlines [
    "# Generated MIPS Assembly Code",
    "# Three-Address Code to MIPS Translation",
    ""
  ]

-- MIPS postamble with exit syscall
mipsPostamble :: String
mipsPostamble = unlines [
    "",
    "  # Exit program",
    "  li $v0, 10",
    "  syscall",
    "",
    ".data",
    "newline: .asciiz \"\\n\""
  ]
