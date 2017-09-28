module Parser.Tests where

import Parser.Impl
import Text.Parsec.Prim
import Test.QuickCheck
import qualified Data.Char as C
import SubsAst

testIdentP s =
  identGood s ==> case parse identP "" s of
    Right s' -> (s==s')
    Left e   -> False

identGood s = (s/= []) && C.isLetter (head s) && ((all (\c -> C.isAlphaNum c || (c == '_')) (tail s)) || (tail s == []))



-- Your white-box tests here
atomTest :: String
atomTest = allTest atomSubsList atomExprList where
    atomSubsList = ["5764","'hello world'","Array(0,1,1)","xs","true","false","undefined","[ for (x of xs) if (x === 5) x ]","['h','e','l']","(x=y,z=x)"]

    atomExprList = [Number 5764,String "hello world",Call "Array" [(Number 0),(Number 1),(Number 1)], Var "xs", TrueConst, FalseConst, Undefined, Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [(Var "x"),(Number 5)]) (ACBody (Var "x")))), Array [(String "h"),(String "e"), (String "l")], Comma (Assign "x" (Var "y")) (Assign "z" (Var "x"))]

operatorTest :: String
operatorTest = allTest opSubsList opExprList where
  opSubsList = ["5,x,4","x=6","4 < 5","4 === 5", "8+3","7-6","8*3","8%3"]

  opExprList = [Comma (Number 5) (Comma (Var "x") (Number 4)),Assign "x" (Number 6), Call "<" [Number 4,Number 5], Call "===" [Number 4,Number 5], Call "+" [Number 8, Number 3], Call "-" [Number 7, Number 6], Call "*" [Number 8, Number 3], Call "%" [Number 8, Number 3]]

opPrecTest :: String
opPrecTest = allTest precSubsList precExprList where
  precSubsList = ["1,x=2+3-4*5%6"]
  precExprList = [Comma (Number 1) (Assign "x" (Call "-" [(Call "+" [Number 2,Number 3]),(Call "%" [(Call "*" [Number 4,Number 5]),Number 6])]))]

opAssocTest :: String
opAssocTest = allTest assocSubsList assocExprList where
  assocSubsList = ["a,b,c","a=b=c","a+b+c","a-b-c","a*b*c","a%b%c"]
  assocExprList = [Comma (Var "a") (Comma (Var "b") (Var "c")), Assign "a" (Assign "b" (Var "c")), Call "+" [Call "+" [Var "a",Var "b"],Var "c"], Call "-" [Call "-" [Var "a",Var "b"],Var "c"], Call "*" [Call "*" [Var "a",Var "b"],Var "c"], Call "%" [Call "%" [Var "a",Var "b"],Var "c"]]

allTest :: [String] -> [Expr] -> String
allTest [] _               = "All Tests Pass"
allTest (x:xs) (expr:exprs) = case parseString x of
  Left e      -> "Test " ++ x ++ " failed to parse"
  Right expr' -> if (expr' == expr)
    then allTest xs exprs
    else "Test " ++ x ++ " was not correct"




-- testing Numbers
testNumberP n =
  (numberGood n) ==> case parseString (show n) of
    Right s -> (s == Number n)
    Left e -> False
  where
    numberGood n = (-10^9 < n) && (n < 10^9)

-- testing strings
-- generate strings
newtype NoEscape = NE String deriving (Show, Eq)
instance Arbitrary NoEscape where
  arbitrary = fmap NE (listOf nonescape)

nonescape = elements(['!'..'@'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "''''''''''")
-- the ascii range of the non-alphanumeric symbols do not include backslash
-- multiple single quotes makes their occurrences likely
-- we need only the closing quote here, the opening one is prepended below

testNoEscape (NE s) =
    (stringGood s) ==> case parseString ('\'':s) of
    Right (String myRes) -> myRes == result
    Left e -> False
    where
      stringGood s = '\'' `elem` s   -- SubsScript string needs to end by '
      result = takeWhile (/= '\'') s -- SubsScript string ends by '



testWhitespaceAfterNumber :: Bool
testWhitespaceAfterNumber =
  case parseString "1234z" of
    Left _  -> True
    Right _ -> False


testWhitespaceInString :: Bool
testWhitespaceInString =
  case parseString "'space allowed'" of
    Right (String "space allowed") -> True
    Left _                         -> False
    Right _                        -> False


testComments :: Bool
testComments =
  case parseString "x = 3 // comment 4 + 4\n" of
    Right (Assign "x" (Number 3)) -> True
    Right _                       -> False
    Left _                        -> False
