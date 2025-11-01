-- Lexer.x
{
module Lexer where
}

%wrapper "posn"

tokens :-
  $white+                 ;
  if                      { tok (const TokenIf) }
  then                    { tok (const TokenThen) }
  else                    { tok (const TokenElse) }
  [a-zA-Z][a-zA-Z0-9]*    { tok TokenID }
  [0-9]+                  { tok (TokenNum . read) }
  \+                      { tok (const TokenPlus) }
  \*                      { tok (const TokenTimes) }
  \(                      { tok (const TokenLParen) }
  \)                      { tok (const TokenRParen) }

{
-- 토큰 타입
data Token
  = Token AlexPosn TokenType
  deriving (Show)

data TokenType
  = TokenIf
  | TokenThen
  | TokenElse
  | TokenID String
  | TokenNum Int
  | TokenPlus
  | TokenTimes
  | TokenLParen
  | TokenRParen
  | TokenEOF
  deriving (Show)

-- 토큰 생성 헬퍼
tok :: (String -> TokenType) -> AlexPosn -> String -> Token
tok f p s = Token p (f s)

posLineCol :: AlexPosn -> (Int, Int)
posLineCol (AlexPn _ l c) = (l, c)

lexer = alexScanTokens
}
