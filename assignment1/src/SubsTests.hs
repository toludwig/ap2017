module SubsTest
    (
    testEquals
    testArithmetic
    testAssignment
    testArray
    testArrayCompr
    ) where

import SubsAst
import SubsInterpreter
import Test.QuickCheck

instance Arbitrary Expr where
  arbitrary = testEquals

testEquals :: Expr -> Bool
testEquals s = evalExpr 
