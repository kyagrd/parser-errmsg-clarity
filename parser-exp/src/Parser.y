-- Parser.y
{
module Parser where
import Lexer
import Types
import System.IO
}

%name stmt
%name expr
%name prog Stmts
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
    'var'   { Token _ TokenVar }
    'fun'   { Token _ TokenFun }
    '{'     { Token _ TokenLBrace }
    '}'     { Token _ TokenRBrace }
    ';'     { Token _ TokenSemicolon }
    ','     { Token _ TokenComma }

%%

Stmt :: { Stmt }
Stmt : Expr ';'          { ExprStmt $1 }
     | 'var' ID ';'      { VarDecl $2 }
     | 'fun' ID '(' Params ')' '{' Stmts '}'  { FunDecl $2 $4 $7 }
     | 'if' Expr 'then' Stmt 'else' Stmt      { IfStmt $2 $4 $6 }
     | '{' Stmts '}'     { Block $2 }

Stmts :: { [Stmt] }
Stmts : Stmts Stmt       { $1 ++ [$2] }
      |                  { [] }

Params :: { [String] }
Params : ID              { [$1] }
       | Params ',' ID   { $1 ++ [$3] }
       |                 { [] }

Args :: { [Expr] }
Args : Expr             { [$1] }
     | Args ',' Expr    { $1 ++ [$3] }
     |                  { [] }

Expr :: { Expr }
Expr : Expr '+' Term    { Plus $1 $3 }
     | Term             { $1 }

Term :: { Expr }
Term : Term '*' Factor  { Times $1 $3 }
     | Factor           { $1 }

Factor :: { Expr }
Factor : ID             { Var $1 }
       | NUM            { Num $1 }
       | '(' Expr ')'   { $2 }
       | ID '(' Args ')' { FunCall $1 $3 }


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
        TokenVar -> "var"
        TokenFun -> "fun"
        TokenLBrace -> "{"
        TokenRBrace -> "}"
        TokenSemicolon -> ";"
        TokenComma -> ","
  in Left $ "Parse error at line " ++ show l ++ ", column " ++ show c ++ " on input '" ++ tokenStr ++ "'"
}
