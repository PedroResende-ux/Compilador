-- AST.hs
-- Definição da Árvore Sintática Abstrata para o subconjunto de Ada

module AST where

-- Programa principal
data Program = Program [Stmt]
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