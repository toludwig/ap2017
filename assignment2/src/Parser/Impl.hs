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
symbolP :: String -> Parser String
symbolP s = do
  spaces
  c <- string s
  return c


-- Parses an expression
exprP :: Parser Expr
exprP = do
  spaces
  expr <- (try (exprMultP) <|> expr1P)
  spaces
  return expr

-- Parses a Comma expression
exprMultP :: Parser Expr
exprMultP = do
  expr1 <- expr1P
  symbolP ","
  expr2 <- exprP
  return (Comma expr1 expr2)

-- Parses a single expression
expr1P :: Parser Expr
expr1P = do
  spaces
  expr <- (try (assignP) <|> term2P)
  return expr

-- Parses an Assign expression
assignP :: Parser Expr
assignP = do
  name <- identP
  symbolP "="
  expr <- expr1P
  return (Assign name expr)

term2P :: Parser Expr
term2P = term3P `chainl1` compOpP

compOpP :: Parser (Expr -> Expr -> Expr)
compOpP = do{ symbolP "==="; return (binOp "===")   }
       <|> do{ symbolP "<"; return (binOp "<") }

binOp :: String -> Expr -> Expr -> Expr
binOp op expr1 expr2 = Call op [expr1,expr2]

term3P :: Parser Expr
term3P = term4P `chainl1` addOpP

addOpP :: Parser (Expr -> Expr -> Expr)
addOpP = do{ symbolP "+"; return (binOp "+")   }
      <|> do{ symbolP "-"; return (binOp "-") }

term4P :: Parser Expr
term4P = atomP `chainl1` multOpP

multOpP :: Parser (Expr -> Expr -> Expr)
multOpP = do{ symbolP "*"; return (binOp "*")   }
      <|> do{ symbolP "%"; return (binOp "%") }

atomP :: Parser Expr
atomP = try numberP
     <|> try stringP
     <|> try trueP
     <|> try falseP
     <|> try undefP
     <|> try varP
     <|> try (do
       symbolP "["
       compr <- arrayForP
       symbolP "]"
       return (Compr compr))
     <|> try arrayP
     <|> parensP

trueP :: Parser Expr
trueP = do
  symbolP "true"
  return TrueConst

falseP :: Parser Expr
falseP = do
  symbolP "false"
  return FalseConst

undefP :: Parser Expr
undefP = do
  symbolP "undefined"
  return Undefined

varP :: Parser Expr
varP = do
  name <- identP
  return (Var name)

arrayForP :: Parser ArrayCompr
arrayForP = do
  symbolP "for"
  symbolP "("
  name <- identP
  symbolP "of"
  expr <- expr1P
  symbolP ")"
  array <- arrayCompP
  return (ACFor name expr array)

exprsP :: Parser [Expr]
exprsP = expr1P `sepBy` (symbolP ",")

arrayCompP :: Parser ArrayCompr
arrayCompP = arrayForP
          <|> arrayIfP
          <|> do
            expr <- expr1P
            return (ACBody expr)

arrayIfP :: Parser ArrayCompr
arrayIfP = do
  symbolP "if"
  symbolP "("
  expr <- expr1P
  symbolP ")"
  compr <- arrayCompP
  return (ACIf expr compr)

arrayP :: Parser Expr
arrayP = do
  symbolP "["
  exprs <- exprsP
  symbolP "]"
  return (Array exprs)

parensP :: Parser Expr
parensP = do
  symbolP "("
  expr <- exprP
  symbolP ")"
  return expr

stringP :: Parser Expr
stringP = fail "Want a string"


keyWords :: [String]
keyWords = ["for","of","true","false","undefined","if"]

numberP :: Parser Expr
numberP = do
  spaces
  (try negP <|> posP)

posP :: Parser Expr
posP =  do
  ns <- many1 digit
  if (length ns < 9)
    then return (Number (read ns))
    else fail "Number too long"

negP :: Parser Expr
negP = do
  x <- string "-"
  ns <- many1 digit
  if (length ns < 9)
    then return (Number (read (x ++ ns)))
    else fail "Number too long"

identP :: Parser String
identP =  do
  spaces
  c <- letter
  cs <- many (alphaNum <|> satisfy ('_' ==))
  if ((c:cs) `elem` keyWords)
    then fail ((c:cs) ++ " is a keyword")
    else return (c:cs)
