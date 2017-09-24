module Parser.Impl where

import SubsAst
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char (ord)
-- Runs the parser on a given string
parseString :: String -> Either ParseError Expr
parseString s = parse exprP "Impl.hs" s
-- Parses a given file
parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile f = do
  s <- readFile f
  return (parseString s)

-- Parser to strip whitespace and then parse a character
symbol :: String -> Parser String
symbol s = do
  spaces
  c <- try (string s)
  return c

-- Parses an expression
exprP :: Parser Expr
exprP = do
  spaces
  (exprMultP <|> expr1P)

-- Parses a Comma expression
exprMultP :: Parser Expr
exprMultP = do
  expr1 <- expr1P
  symbol ","
  expr2 <- exprP
  return (Comma expr1 expr2)

-- Parses a single expression
expr1P :: Parser Expr
expr1P = assignP
      <|> term2P

-- Parses an Assign expression
assignP :: Parser Expr
assignP = do
  name <- identP
  symbol "="
  expr <- expr1P
  return (Assign name expr)

term2P :: Parser Expr
term2P = binOpP "===" term3P
      <|> binOpP "<" term3P
      <|> term3P

binOpP :: String -> Parser Expr -> Parser Expr
binOpP op p = do
  expr1 <- p
  s <- symbol op
  expr2 <- expr1P
  return (Call s [expr1,expr2])

term3P :: Parser Expr
term3P = binOpP "+" term4P
      <|> binOpP "-" term4P
      <|> term4P

term4P :: Parser Expr
term4P = binOpP "*" atomP
      <|> binOpP "%" atomP
      <|> atomP

atomP :: Parser Expr
atomP = numberP
     <|> stringP
     <|> trueP
     <|> falseP
     <|> undefP
     <|> varP
     <|> (do
       symbol "["
       compr <- arrayForP
       symbol "]"
       return (Compr compr))
     <|> arrayP
     <|> parensP

trueP :: Parser Expr
trueP = do
  symbol "true"
  return TrueConst

falseP :: Parser Expr
falseP = do
  symbol "false"
  return FalseConst

undefP :: Parser Expr
undefP = do
  symbol "undefined"
  return Undefined

varP :: Parser Expr
varP = do
  name <- identP
  return (Var name)

arrayForP :: Parser ArrayCompr
arrayForP = do
  symbol "for"
  symbol "("
  name <- identP
  symbol "of"
  expr <- expr1P
  symbol ")"
  array <- arrayCompP
  return (ACFor name expr array)

exprsP :: Parser [Expr]
exprsP = expr1P `sepBy` (symbol ",")

arrayCompP :: Parser ArrayCompr
arrayCompP = arrayForP
          <|> arrayIfP
          <|> do
            expr <- expr1P
            return (ACBody expr)

arrayIfP :: Parser ArrayCompr
arrayIfP = do
  symbol "if"
  symbol "("
  expr <- expr1P
  symbol ")"
  compr <- arrayCompP
  return (ACIf expr compr)

arrayP :: Parser Expr
arrayP = do
  symbol "["
  exprs <- exprsP
  symbol "]"
  return (Array exprs)

parensP :: Parser Expr
parensP = do
  symbol "("
  expr <- exprP
  symbol ")"
  return expr

stringP :: Parser Expr
stringP = undefined


keyWords :: [String]
keyWords = ["for","of","true","false","undefined","if"]

numberP :: Parser Expr
numberP = negP
       <|> posP

negP :: Parser Expr
negP = do
  x <- symbol "-"
  ns <- many1 digit
  if (length ns < 9)
    then return (Number (read (x ++ ns)))
    else fail "Number too long"

posP :: Parser Expr
posP = undefined

identP :: Parser String
identP = do
  spaces
  c <- letter
  cs <- many (alphaNum <|> satisfy ('_' ==))
  if ((c:cs) `elem` keyWords)
    then fail ((c:cs) ++ " is a keyword")
    else return (c:cs)
