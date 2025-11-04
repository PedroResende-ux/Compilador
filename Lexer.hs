{
-- Lexer.x
-- Analisador léxico para o subconjunto de Ada

module Lexer where
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
$alphaNum = [$alpha $digit]

tokens :-

  -- Ignorar espaços em branco e comentários
  $white+                              ;
  "--".*                               ; -- comentários de linha
  
  -- Palavras reservadas (case-insensitive em Ada, mas simplificamos)
  procedure                            { \p s -> TokenProcedure p }
  is                                   { \p s -> TokenIs p }
  begin                                { \p s -> TokenBegin p }
  end                                  { \p s -> TokenEnd p }
  if                                   { \p s -> TokenIf p }
  then                                 { \p s -> TokenThen p }
  else                                 { \p s -> TokenElse p }
  while                                { \p s -> TokenWhile p }
  loop                                 { \p s -> TokenLoop p }
  and                                  { \p s -> TokenAnd p }
  or                                   { \p s -> TokenOr p }
  not                                  { \p s -> TokenNot p }
  mod                                  { \p s -> TokenMod p }
  True                                 { \p s -> TokenTrue p }
  False                                { \p s -> TokenFalse p }
  Put_Line                             { \p s -> TokenPutLine p }
  Get_Line                             { \p s -> TokenGetLine p }
  
  -- Operadores e pontuação
  ":="                                 { \p s -> TokenAssign p }
  ";"                                  { \p s -> TokenSemi p }
  ":"                                  { \p s -> TokenColon p }
  "("                                  { \p s -> TokenLParen p }
  ")"                                  { \p s -> TokenRParen p }
  "+"                                  { \p s -> TokenPlus p }
  "-"                                  { \p s -> TokenMinus p }
  "*"                                  { \p s -> TokenTimes p }
  "/"                                  { \p s -> TokenDiv p }
  "="                                  { \p s -> TokenEq p }
  "/="                                 { \p s -> TokenNeq p }
  "<"                                  { \p s -> TokenLt p }
  "<="                                 { \p s -> TokenLte p }
  ">"                                  { \p s -> TokenGt p }
  ">="                                 { \p s -> TokenGte p }
  
  -- Literais
  $digit+                              { \p s -> TokenInt p (read s) }
  \"[^\"]*\"                           { \p s -> TokenString p (init (tail s)) }
  
  -- Identificadores (devem vir depois das palavras reservadas)
  $alpha [$alphaNum \_]*               { \p s -> TokenId p s }

{

-- Tipo de tokens
data Token = 
    TokenProcedure AlexPosn
  | TokenIs AlexPosn
  | TokenBegin AlexPosn
  | TokenEnd AlexPosn
  | TokenIf AlexPosn
  | TokenThen AlexPosn
  | TokenElse AlexPosn
  | TokenWhile AlexPosn
  | TokenLoop AlexPosn
  | TokenAnd AlexPosn
  | TokenOr AlexPosn
  | TokenNot AlexPosn
  | TokenMod AlexPosn
  | TokenTrue AlexPosn
  | TokenFalse AlexPosn
  | TokenPutLine AlexPosn
  | TokenGetLine AlexPosn
  | TokenAssign AlexPosn
  | TokenSemi AlexPosn
  | TokenColon AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenDiv AlexPosn
  | TokenEq AlexPosn
  | TokenNeq AlexPosn
  | TokenLt AlexPosn
  | TokenLte AlexPosn
  | TokenGt AlexPosn
  | TokenGte AlexPosn
  | TokenInt AlexPosn Int
  | TokenString AlexPosn String
  | TokenId AlexPosn String
  | TokenEOF
  deriving (Show, Eq)

-- Função auxiliar para extrair posição de um token
getTokenPos :: Token -> String
getTokenPos tok = case tok of
  TokenProcedure p -> showPosn p
  TokenIs p -> showPosn p
  TokenBegin p -> showPosn p
  TokenEnd p -> showPosn p
  TokenIf p -> showPosn p
  TokenThen p -> showPosn p
  TokenElse p -> showPosn p
  TokenWhile p -> showPosn p
  TokenLoop p -> showPosn p
  TokenAnd p -> showPosn p
  TokenOr p -> showPosn p
  TokenNot p -> showPosn p
  TokenMod p -> showPosn p
  TokenTrue p -> showPosn p
  TokenFalse p -> showPosn p
  TokenPutLine p -> showPosn p
  TokenGetLine p -> showPosn p
  TokenAssign p -> showPosn p
  TokenSemi p -> showPosn p
  TokenColon p -> showPosn p
  TokenLParen p -> showPosn p
  TokenRParen p -> showPosn p
  TokenPlus p -> showPosn p
  TokenMinus p -> showPosn p
  TokenTimes p -> showPosn p
  TokenDiv p -> showPosn p
  TokenEq p -> showPosn p
  TokenNeq p -> showPosn p
  TokenLt p -> showPosn p
  TokenLte p -> showPosn p
  TokenGt p -> showPosn p
  TokenGte p -> showPosn p
  TokenInt p _ -> showPosn p
  TokenString p _ -> showPosn p
  TokenId p _ -> showPosn p
  TokenEOF -> "EOF"

showPosn :: AlexPosn -> String
showPosn (AlexPn _ line col) = "line " ++ show line ++ ", column " ++ show col

}