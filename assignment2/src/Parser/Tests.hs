module Parser.Tests where

import Parser.Impl
import SubsAst
import Test.QuickCheck
import Text.Parsec.Prim





testNumberP n =
  (numberGood n) ==> case parse numberP "" (show n) of
    Right s -> (s == Number n)
    Left e -> False
  where
    numberGood n = (-10^9 < n) && (n < 10^9)

instance Arbitrary EscapedString where
arbitrary = "\\" ++ oneof [escNL, escT, escN, esc', esc\]

escNL = fmap 

testStringP =
   ==> case parse numberP "" (show n) of
    Right s' -> (s' == String s)
    Left e -> False
