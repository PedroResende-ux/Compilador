-- AST.hs
-- Definição da Árvore Sintática Abstrata para o subconjunto de Ada

module AST where

import qualified Data.Map as Map
import Data.Maybe (isJust)

-- Tipos de variáveis
data Type = IntegerType | BooleanType
  deriving (Show, Eq)

-- Informação de símbolos
data SymbolInfo = SymbolInfo 
  { symbolName :: String
  , symbolType :: Type
  , scopeLevel :: Int
  }
  deriving (Show, Eq)

-- Tabela de símbolos (usa Map para eficiência)
type Scope = Map.Map String SymbolInfo
data SymbolTable = SymbolTable
  { scopes :: [Scope]          -- Lista de escopos (topo = escopo atual)
  , currentLevel :: Int         -- Nível de escopo atual
  }
  deriving (Show, Eq)

-- Programa principal com tabela de símbolos
data Program = Program [Decl] [Stmt]
  deriving (Show, Eq)

-- Declarações
data Decl = VarDecl String Type
  deriving (Show, Eq)

-- Comandos (Statements)
data Stmt = 
    Assignment String Expr              -- x := expr
  | IfThenElse Expr Stmt Stmt          -- if expr then stmt else stmt
  | IfThen Expr Stmt                   -- if expr then stmt (sem else)
  | While Expr Stmt                    -- while expr loop stmt end loop
  | Block [Stmt]                       -- begin stmt1; stmt2; ... end
  | PutLine Expr                       -- Put_Line(expr)
  | EmptyStmt                          -- comando vazio
  deriving (Show, Eq)

-- Expressões
data Expr = 
    -- Literais
    IntLit Int                         -- 42
  | BoolLit Bool                       -- True, False
  | StringLit String                   -- "texto"
  | Var String                         -- identificador
    
    -- Operações aritméticas
  | Add Expr Expr                      -- e1 + e2
  | Sub Expr Expr                      -- e1 - e2
  | Mul Expr Expr                      -- e1 * e2
  | Div Expr Expr                      -- e1 / e2
  | Mod Expr Expr                      -- e1 mod e2
  | Neg Expr                           -- -e
    
    -- Operações booleanas
  | And Expr Expr                      -- e1 and e2
  | Or Expr Expr                       -- e1 or e2
  | Not Expr                           -- not e
    
    -- Operações relacionais
  | Eq Expr Expr                       -- e1 = e2
  | Neq Expr Expr                      -- e1 /= e2
  | Lt Expr Expr                       -- e1 < e2
  | Lte Expr Expr                      -- e1 <= e2
  | Gt Expr Expr                       -- e1 > e2
  | Gte Expr Expr                      -- e1 >= e2
    
    -- Input
  | GetLine                            -- Get_Line
  deriving (Show, Eq)

--TAC

data TAC =
    Assign String String                   -- x := y (Direct Assignment)
  | BinOp String String String String      -- x := y op z (e.g., "Add", "Sub")
  | UnOp String String String              -- x := op y (e.g., "Neg")
  | Goto String                            -- goto label
  | Ifz String String                      -- ifz x goto label (Conditional Jump)
  | Label String                           -- label: (Control Flow Marker)
  deriving (Show, Eq)

-- Symbol Table Operations

-- Create an empty symbol table
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Map.empty] 0

-- Enter a new scope (e.g., begin block)
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes level) = 
  SymbolTable (Map.empty : scopes) (level + 1)

-- Exit current scope (e.g., end block)
exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable [] _) = error "Cannot exit global scope - symbol table corrupted"
exitScope (SymbolTable [_] 0) = error "Cannot exit global scope"
exitScope (SymbolTable (_:rest) level) = 
  SymbolTable rest (level - 1)

-- Insert a symbol into the current scope
-- Returns Nothing if symbol already exists in current scope, Just table otherwise
insertSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable
insertSymbol _ _ (SymbolTable [] _) = error "Cannot insert into empty symbol table"
insertSymbol name typ (SymbolTable (currentScope:rest) level) =
  if Map.member name currentScope
  then Nothing  -- Symbol already declared in current scope
  else let info = SymbolInfo name typ level
           newScope = Map.insert name info currentScope
       in Just (SymbolTable (newScope:rest) level)

-- Lookup a symbol in all scopes (search from current to global)
lookupSymbol :: String -> SymbolTable -> Maybe SymbolInfo
lookupSymbol name (SymbolTable scopes _) = 
  lookupInScopes name scopes
  where
    lookupInScopes :: String -> [Scope] -> Maybe SymbolInfo
    lookupInScopes _ [] = Nothing
    lookupInScopes n (s:ss) = 
      case Map.lookup n s of
        Just info -> Just info
        Nothing -> lookupInScopes n ss

-- Check if symbol is declared
isDeclared :: String -> SymbolTable -> Bool
isDeclared name st = isJust (lookupSymbol name st)