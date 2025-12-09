{
-- Parser.y
-- Analisador sintático para o subconjunto de Ada

module Parser where
import Lexer
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  procedure { TokenProcedure _ }
  is        { TokenIs _ }
  begin     { TokenBegin _ }
  end       { TokenEnd _ }
  if        { TokenIf _ }
  then      { TokenThen _ }
  else      { TokenElse _ }
  while     { TokenWhile _ }
  loop      { TokenLoop _ }
  and       { TokenAnd _ }
  or        { TokenOr _ }
  not       { TokenNot _ }
  mod       { TokenMod _ }
  true      { TokenTrue _ }
  false     { TokenFalse _ }
  put_line  { TokenPutLine _ }
  get_line  { TokenGetLine _ }
  inttype   { TokenIntegerType _ }
  booltype  { TokenBooleanType _ }
  ':='      { TokenAssign _ }
  ';'       { TokenSemi _ }
  ':'       { TokenColon _ }
  '('       { TokenLParen _ }
  ')'       { TokenRParen _ }
  '+'       { TokenPlus _ }
  '-'       { TokenMinus _ }
  '*'       { TokenTimes _ }
  '/'       { TokenDiv _ }
  '='       { TokenEq _ }
  '/='      { TokenNeq _ }
  '<'       { TokenLt _ }
  '<='      { TokenLte _ }
  '>'       { TokenGt _ }
  '>='      { TokenGte _ }
  int       { TokenInt _ $$ }
  string    { TokenString _ $$ }
  id        { TokenId _ $$ }
 
-- $$ signfica o valor associado ao token e _ é a posição do token
-- usamos apenas _ porque o happy exige que todos os tokens tenham posição

-- Precedências e associatividades
%right ':='
%left or
%left and
%nonassoc '=' '/=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' mod
%right not NEG  -- NEG é para menos unário

%%

-- Programa principal: procedure Main is [declarations] begin ... end Main;
Program : procedure id is DeclList begin StmtList end id ';' 
          { if $2 /= "Main" || $2 /= $8 
            then error "Procedure name must be 'Main' and match at begin/end"
            else Program $4 $6 
          }
        | procedure id is begin StmtList end id ';' 
          { if $2 /= "Main" || $2 /= $7 
            then error "Procedure name must be 'Main' and match at begin/end"
            else Program [] $5 
          }

-- Lista de declarações
DeclList : Decl                    { [$1] }
         | Decl DeclList           { $1 : $2 }

-- Declaração de variável: x : Integer;
Decl : id ':' Type ';'             { VarDecl $1 $3 }

-- Tipos
Type : inttype                     { IntegerType }
     | booltype                    { BooleanType }

-- Lista de comandos separados por ;
StmtList : Stmt                    { [$1] }
         | Stmt ';' StmtList       { $1 : $3 }

-- Comandos
Stmt : id ':=' Expr                           { Assignment $1 $3 }
     | if Expr then Stmt else Stmt            { IfThenElse $2 $4 $6 }
     | if Expr then Stmt                      { IfThen $2 $4 }
     | while Expr loop StmtList end loop      { While $2 (Block $4) }
     | begin StmtList end                     { Block $2 }
     | put_line '(' Expr ')'                  { PutLine $3 }
     | {- vazio -}                            { EmptyStmt }

-- Expressões booleanas
Expr : Expr or Expr                { Or $1 $3 }
     | Expr and Expr               { And $1 $3 }
     | not Expr                    { Not $2 }
     | CompExpr                    { $1 }

-- Expressões de comparação
CompExpr : ArithExpr '=' ArithExpr   { Eq $1 $3 }
         | ArithExpr '/=' ArithExpr  { Neq $1 $3 }
         | ArithExpr '<' ArithExpr   { Lt $1 $3 }
         | ArithExpr '<=' ArithExpr  { Lte $1 $3 }
         | ArithExpr '>' ArithExpr   { Gt $1 $3 }
         | ArithExpr '>=' ArithExpr  { Gte $1 $3 }
         | ArithExpr                 { $1 }

-- Expressões aritméticas
ArithExpr : ArithExpr '+' ArithExpr  { Add $1 $3 }
          | ArithExpr '-' ArithExpr  { Sub $1 $3 }
          | ArithExpr '*' ArithExpr  { Mul $1 $3 }
          | ArithExpr '/' ArithExpr  { Div $1 $3 }
          | ArithExpr mod ArithExpr  { Mod $1 $3 }
          | Term                     { $1 }

-- Termos
Term : int                       { IntLit $1 }
     | true                      { BoolLit True }
     | false                     { BoolLit False }
     | string                    { StringLit $1 }
     | id                        { Var $1 }
     | get_line                  { GetLine }
     | '-' Term %prec NEG        { Neg $2 }
     | '(' Expr ')'              { $2 }

{

parseError :: [Token] -> a
parseError [] = error "Parse error at end of file"
parseError (t:ts) = error $ "Parse error at " ++ getTokenPos t

}