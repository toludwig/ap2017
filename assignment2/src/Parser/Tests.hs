module Parser.Tests where

import Parser.Impl
import SubsAst
import Test.QuickCheck
import Text.Parsec.Prim


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
