-- Parser.y
{
module Parser where
import Lexer
import Types
import System.IO
}

%name parser
%tokentype { Token }
%error { parseError }
%monad { Either String }

%token
    ID      { Token _ (TokenID $$) }
    NUM     { Token _ (TokenNum $$) }
    '+'     { Token _ TokenPlus }
    '*'     { Token _ TokenTimes }
    '('     { Token _ TokenLParen }
    ')'     { Token _ TokenRParen }
    'if'    { Token _ TokenIf }
    'then'  { Token _ TokenThen }
    'else'  { Token _ TokenElse }

%%
Expr :: { Expr }
Expr : Expr '+' Term    { Plus $1 $3 }
     | Term             { $1 }
     | IfTE             { $1 }
     
IfTE :: { Expr }
IFTE : 'if' Expr 'then' Expr 'else' Expr    { If $2 $4 $6 }

Term :: { Expr }
Term : Term '*' Factor  { Times $1 $3 }
     | Factor           { $1 }

Factor :: { Expr }
Factor : ID             { Var $1 }
       | NUM            { Num $1 }
       | '(' Expr ')'   { $2 }


{
type ParserResult a = Either String a
-- parseError :: [Token] -> a
-- parseError [] = error "ParseError: Empty token stream."
-- parseError (tok:_) = error $ "ParseError: Unexpected token '" <> show tok <> "'."

parseError :: [Token] -> ParserResult a
parseError [] = Left "Parse error at end of input"
parseError (Token (AlexPn _ l c) t : _) =
  let tokenStr = case t of
        TokenID s -> s
        TokenNum n -> show n
        TokenPlus -> "+"
        TokenTimes -> "*"
        TokenLParen -> "("
        TokenRParen -> ")"
        TokenIf -> "if"
        TokenThen -> "then"
        TokenElse -> "else"
  in Left $ "Parse error at line " ++ show l ++ ", column " ++ show c ++ " on input '" ++ tokenStr ++ "'"
}
