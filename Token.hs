module Token where

data Token
  = TokIf
  | TokThen
  | TokElse
  | TokWhile
  | TokLoop
  | TokProcedure
  | TokIs
  | TokBegin
  | TokEnd
  | TokPutLine
  | TokGetLine
  | TokIdent String
  | TokInt Int
  | TokString String
  | TokAssign      -- :=
  | TokPlus        -- +
  | TokMinus       -- -
  | TokMul         -- *
  | TokDiv         -- /
  | TokEq          -- =
  | TokNeq         -- /=
  | TokLt | TokLe | TokGt | TokGe
  | TokLParen | TokRParen
  | TokSemicolon
  deriving (Show, Eq)
