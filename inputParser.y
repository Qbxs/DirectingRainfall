{
module InputParser where
}

%name inputParser
%tokentype { Token }
%error { parseError }

%token
      int             { TokenInt $$ }

%%

Input    :: { Input }
         : Range Tarps Tarplist    { Input $1 $2 $3 }

Range    :: { Range }
         : int int                 { ($1, $2) }

Tarps    :: { Int }
         : int                     { $1 }

Tarplist :: { [Tarp] }
         : Tarp                    { [$1] }
         | Tarp Tarplist           { $1:$2 }

Tarp     :: { Tarp }
         : Point Point             { T $1 $2 }

Point    :: { Point }
         : int int                 { ($1,$2) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Input = Input
  { vineyard  :: Range
  , noOfTarps :: Int
  , tarps     :: [Tarp]
  } deriving(Show)

type Point = (Int, Int)

type Range = (Int, Int)

data Tarp = T Point Point
 deriving (Show,Eq)

data Token = TokenInt Int
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isDigit c = lexNum (c:cs)

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

isDigit x = elem x ['0'..'9']
isSpace x = x == ' ' || x == '\n'
}
