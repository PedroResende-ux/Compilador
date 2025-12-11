-- AST.hs
-- Definição da árvore sintática abstrata para o subconjunto de Ada
--
-- Implementa:
--   - Tipos de dados da AST para comandos e expressões
--   - Tabela de símbolos com gestão de escopos
--   - Sistema de tipos (Integer, Boolean)
--   - Tipos de dados para código de três endereços (TAC)
--
-- Operações da Tabela de Símbolos:
--   - emptySymbolTable: Inicializar tabela vazia
--   - insertSymbol: Adicionar símbolo ao escopo atual
--   - lookupSymbol: Procurar símbolo em todos os escopos
--   - enterScope: Entrar num novo bloco de escopo
--   - exitScope: Sair do escopo atual
--   - isDeclared: Verificar se variável está declarada
--
-- Implementação usa Data.Map para operações eficientes O(log n).

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
    Assignment String Expr
  | IfThenElse Expr Stmt Stmt
  | IfThen Expr Stmt
  | While Expr Stmt
  | Block [Stmt]
  | PutLine Expr
  | EmptyStmt
  deriving (Show, Eq)

-- Expressões
data Expr = 
    -- Literais
    IntLit Int
  | BoolLit Bool
  | StringLit String
  | Var String
    
    -- Operações aritméticas
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Neg Expr
    
    -- Operações booleanas
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
    
    -- Operações relacionais
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Lte Expr Expr
  | Gt Expr Expr
  | Gte Expr Expr
    
    -- Input
  | GetLine
  deriving (Show, Eq)

-- Código de Três Endereços (TAC)
-- Usado como representação intermédia entre AST e MIPS

data Instr =
    Assign String String                   -- x := y (atribuição direta)
  | BinOp String String String String      -- x := y op z
  | UnOp String String String              -- x := op y
  | Goto String                            -- Salto incondicional: goto label
  | Ifz String String                      -- Salto condicional: ifz var label
  | Label String                           -- Marcador de rótulo para saltos
  deriving (Show, Eq)

-- Operações sobre a Tabela de Símbolos

-- Criar uma tabela de símbolos vazia
-- Inicializa uma tabela de símbolos vazia
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Map.empty] 0

-- Entra num novo âmbito/scope
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes level) = 
  SymbolTable (Map.empty : scopes) (level + 1)

-- Sai do âmbito atual
exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable [] _) = error "Cannot exit global scope - symbol table corrupted"
exitScope (SymbolTable [_] 0) = error "Cannot exit global scope"
exitScope (SymbolTable (_:rest) level) = 
  SymbolTable rest (level - 1)

-- Inserir um símbolo no âmbito atual
-- Insere identificador com a respetiva informação
-- Retorna Nothing se símbolo já existe no âmbito atual, Just table caso contrário
insertSymbol :: String -> Type -> SymbolTable -> Maybe SymbolTable
insertSymbol _ _ (SymbolTable [] _) = error "Cannot insert into empty symbol table"
insertSymbol name typ (SymbolTable (currentScope:rest) level) =
  if Map.member name currentScope
  then Nothing  -- Símbolo já declarado no âmbito atual
  else let info = SymbolInfo name typ level
           newScope = Map.insert name info currentScope
       in Just (SymbolTable (newScope:rest) level)

-- Procurar um símbolo em todos os âmbitos (do atual ao global)
-- Procura informação associada a um identificador
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

-- Verificar se símbolo está declarado (função auxiliar baseada em lookupSymbol)
isDeclared :: String -> SymbolTable -> Bool
isDeclared name st = isJust (lookupSymbol name st)