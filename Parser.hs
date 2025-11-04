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
  procedure { TokenProcedure $$ }
  is        { TokenIs $$ }
  begin     { TokenBegin $$ }
  end       { TokenEnd $$ }
  if        { TokenIf $$ }
  then      { TokenThen $$ }
  else      { TokenElse $$ }
  while     { TokenWhile $$ }
  loop      { TokenLoop $$ }
  and       { TokenAnd $$ }
  or        { TokenOr $$ }
  not       { TokenNot $$ }
  mod       { TokenMod $$ }
  true      { TokenTrue $$ }
  false     { TokenFalse $$ }
  put_line  { TokenPutLine $$ }
  get_line  { TokenGetLine $$ }
  ':='      { TokenAssign $$ }
  ';'       { TokenSemi $$ }
  ':'       { TokenColon $$ }
  '('       { TokenLParen $$ }
  ')'       { TokenRParen $$ }
  '+'       { TokenPlus $$ }
  '-'       { TokenMinus $$ }
  '*'       { TokenTimes $$ }
  '/'       { TokenDiv $$ }
  '='       { TokenEq $$ }
  '/='      { TokenNeq $$ }
  '<'       { TokenLt $$ }
  '<='      { TokenLte $$ }
  '>'       { TokenGt $$ }
  '>='      { TokenGte $$ }
  int       { TokenInt _ $$ }
  string    { TokenString _ $$ }
  id        { TokenId _ $$ }

-- Precedências e associatividades
%right ':='
%left or
%left and
%nonassoc '=' '/=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' mod
%right not NEG  -- NEG é para menos unário

%%

-- Programa principal: procedure Main is begin ... end Main;
Program : procedure id is begin StmtList end id ';' 
          { if $2 /= "Main" || $2 /= $7 
            then error "Procedure name must be 'Main' and match at begin/end"
            else Program $5 
          }

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