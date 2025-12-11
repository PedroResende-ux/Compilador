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

-- Código de Três Endereços (TAC)
-- Usado como representação intermédia entre AST e MIPS

data Instr =
    Assign String String                   -- x := y (atribuição direta)
  | BinOp String String String String      -- x := y op z (ex: "Add", "Sub", "Mul")
  | UnOp String String String              -- x := op y (ex: "Neg", "Not")
  | Goto String                            -- Salto incondicional: goto label
  | Ifz String String                      -- Salto condicional: ifz var label
  | Label String                           -- Marcador de rótulo para saltos
  deriving (Show, Eq)

-- Operações sobre a Tabela de Símbolos

-- Criar uma tabela de símbolos vazia
-- (Aula 8: "inicializar uma tabela vazia")
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [Map.empty] 0

-- Entrar num novo âmbito/scope (ex: bloco begin)
-- (Aula 8: "abrir - iniciar num novo âmbito")
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable scopes level) = 
  SymbolTable (Map.empty : scopes) (level + 1)

-- Sair do âmbito atual (ex: bloco end)
-- (Aula 8: "fechar - terminar o âmbito atual")
exitScope :: SymbolTable -> SymbolTable
exitScope (SymbolTable [] _) = error "Cannot exit global scope - symbol table corrupted"
exitScope (SymbolTable [_] 0) = error "Cannot exit global scope"
exitScope (SymbolTable (_:rest) level) = 
  SymbolTable rest (level - 1)

-- Inserir um símbolo no âmbito atual
-- (Aula 8: "inserir dado o identificador e informação")
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
-- (Aula 8: "procurar dado o identificador" - menciona "lookup")
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