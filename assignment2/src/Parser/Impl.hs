module Parser.Impl (parseString,
                    ParseError) where

import SubsAst
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator
import Data.Char
-- Runs the parser on a given string
parseString :: String -> Either ParseError Expr
parseString = parse (discards *> exprP <* eof) "Impl.hs"
-- Parses a given file
parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile f = do
  s <- readFile f
  return (parseString s)

-- Parser to
symbolP :: String -> Parser String
symbolP s = do
  c <- string s
  discards
  return c

discards :: Parser ()
discards = do
  many discard1
  return ()

discard1 :: Parser ()
discard1 =
  (space >> return ()) <|> commentP

commentP :: Parser ()
commentP = do
  symbolP "//"
  manyTill anyChar (string "\n")
  return ()

-- Parses an expression
exprP :: Parser Expr
exprP = try commaP
     <|> expr1P

-- Parses a Comma expression
commaP :: Parser Expr
commaP = do
  expr1 <- expr1P
  symbolP ","
  expr2 <- exprP
  return (Comma expr1 expr2)

-- Parses a single expression
expr1P :: Parser Expr
expr1P = try assignP
      <|> term2P

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
compOpP = do{ s <- symbolP "==="; return (binOp s)   }
       <|> do{ s <- symbolP "<"; return (binOp s) }

binOp :: String -> Expr -> Expr -> Expr
binOp op expr1 expr2 = Call op [expr1,expr2]

term3P :: Parser Expr
term3P = term4P `chainl1` addOpP

addOpP :: Parser (Expr -> Expr -> Expr)
addOpP = do{s <- symbolP "+"; return (binOp s)   }
      <|> do{s <- symbolP "-"; return (binOp s) }

term4P :: Parser Expr
term4P = atomP `chainl1` multOpP

multOpP :: Parser (Expr -> Expr -> Expr)
multOpP = do{s <- symbolP "*"; return (binOp s)   }
      <|> do{s <- symbolP "%"; return (binOp s) }

atomP :: Parser Expr
atomP = try numberP
     <|> try stringP
     <|> try callP
     <|> try varP
     <|> try trueP
     <|> try falseP
     <|> try undefP
     <|> try (do
       symbolP "["
       compr <- arrayForP
       symbolP "]"
       return (Compr compr))
     <|> try arrayP
     <|> parensP

callP :: Parser Expr
callP = do
  name <- identP
  symbolP "("
  exprs <- exprsP
  symbolP ")"
  return (Call name exprs)

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
  string "of"
  discard1
  expr <- expr1P
  symbolP ")"
  array <- arrayCompP
  return (ACFor name expr array)

exprsP :: Parser [Expr]
exprsP = expr1P `sepBy` (symbolP ",")

arrayCompP :: Parser ArrayCompr
arrayCompP = try arrayForP
          <|> try arrayIfP
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
stringP = do
  string "'" -- start of string
  c <- manyTill substringP endstringP
  return (String (concat c))

endstringP :: Parser ()
endstringP = do
  notFollowedBy (char '\\')
  string "'"
  return ()

substringP :: Parser String
substringP = do
  c1 <- anyChar
  case [c1] of
    "\\" -> do              -- single backslash escapes the next char
        c2 <- anyChar       -- which requires reading
        case c2 of
          '\n' -> return ""
          '\\' -> return "\\"
          '\'' -> return "'"
          'n'  -> return "\n"
          't'  -> return "\t"
          _    -> fail "unknown escape sequence"

    _ -> return [c1]    -- otherwise, just return the char


keyWords :: [String]
keyWords = ["for","of","true","false","undefined","if"]

numberP :: Parser Expr
numberP = (try negP <|> posP) <* discards

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
  c <- letter
  cs <- many (alphaNum <|> satisfy ('_' ==))
  discards
  if ((c:cs) `elem` keyWords)
    then fail ((c:cs) ++ " is a keyword")
    else return (c:cs)
