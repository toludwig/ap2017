module Parser.Impl where

import SubsAst
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator

parseString :: String -> Either ParseError Expr
parseString s = parse exprP "Impl.hs" s

exprP :: Parser Expr
exprP = parseNumber

keyWords :: [String]
keyWords = undefined

parseNumber :: Parser Expr
parseNumber = parseNeg
           <|> parsePos

parseNeg :: Parser Expr
parseNeg = do
  x <- string "-"
  ns <- many1 digit
  if (length ns < 9)
    then return (Number (read (x ++ ns)))
    else fail "Number too long"

parsePos :: Parser Expr
parsePos = undefined
