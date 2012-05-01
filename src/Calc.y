{
module Calc (runCalc) where

import qualified Data.Char as Char
}

%name calc
%tokentype { Token }
%error { parseError }

%token 
      Int             { TokenInt $$ }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }

%%

exp :: { Int }
      : exp '+' term            { $1 + $3 }
      | exp '-' term            { $1 - $3 }
      | term                    { $1 }

term :: { Int }
      : term '*' factor         { $1 * $3 }
      | term '/' factor         { $1 `div` $3 }
      | factor                  { $1 }

factor :: { Int }
      : Int                     { $1 }
      | '(' exp ')'             { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenInt Int
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c : cs) 
      | Char.isSpace c = lexer cs
      | Char.isDigit c = lexNum (c : cs)
lexer ('+' : cs) = TokenPlus : lexer cs
lexer ('-' : cs) = TokenMinus : lexer cs
lexer ('*' : cs) = TokenTimes : lexer cs
lexer ('/' : cs) = TokenDiv : lexer cs
lexer ('(' : cs) = TokenOB : lexer cs
lexer (')' : cs) = TokenCB : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
  where (num, rest) = span Char.isDigit cs

runCalc :: String -> Int
runCalc = calc . lexer
}
