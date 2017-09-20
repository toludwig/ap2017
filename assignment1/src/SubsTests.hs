module SubsTest
    ( testPlus
    , testMult
    , testConcat
    , testAssignInt
    , testAssignString
    , testAssignArray
    , testArrayScope
    , testArrayFor
    , testArrayForIf
    ) where

import SubsAst
import SubsInterpreter

-- Testing Context independent functions

testPlus :: Int -> Int -> Bool
testPlus i1 i2 =
  case runExpr (Call "+" [(Number i1), (Number i2)]) of
    Left e    -> False
    Right val -> val == IntVal (i1 + i2)

testMult :: Int -> Int -> Bool
testMult i1 i2 =
  case runExpr (Call "*" [(Number i1), (Number i2)]) of
    Left e    -> False
    Right val -> val == IntVal (i1 * i2)

testConcat :: String -> String -> Bool
testConcat s1 s2 =
  case runExpr (Call "+" [(String s1), (String s2)]) of
    Left e    -> False
    Right val -> val == StringVal (s1 ++ s2)


-- testing Assign

testAssignInt :: Bool
testAssignInt =
  case runExpr (Comma (Assign "a" (Number 1)) -- assigning a, evals to 1
               (Comma (Assign "b" (Number 2)) -- assigning b, evals to 2
                      (Var "a"))) of          -- expression a evals to 1
    Left e  -> False
    Right v -> v == IntVal 1

testAssignString :: Bool
testAssignString =
  case runExpr (Comma (Assign "a" (String "1")) -- assigning a, evals to 1
               (Comma (Assign "b" (String "2")) -- assigning b, evals to 2
                      (Var "a"))) of            -- expression a evals to 1
    Left e  -> False
    Right v -> v == StringVal "1"

testAssignArray :: Bool
testAssignArray =
  case runExpr (Comma (Assign "a" (Array [Number 1])) -- assigning a, evals to [1]
               (Comma (Assign "b" (Array [Number 2])) -- assigning b, evals to [2]
                      (Var "a"))) of                  -- expression a evals to [1]
    Left e  -> False
    Right v -> v == ArrayVal [IntVal 1]


-- Testing Array Comprehensions

testArrayFor :: Bool
testArrayFor = -- calculating the squares of 1, 2 and 3
  case runExpr (Compr (ACFor "x" (Array [Number 1, Number 2, Number 3])
                   (ACBody (Call "*" [Var "x",Var "x"])))) of
    Left e -> False
    Right v -> v == ArrayVal [IntVal 1, IntVal 4, IntVal 9]

testArrayForIf :: Bool
testArrayForIf = -- calculating the squares of even numbers in [1, 2, 3]
  case runExpr (Compr (ACFor "x" (Array [Number 1, Number 2, Number 3])
                      (ACIf (Call "===" [Call "%" [Var "x", Number 2], Number 0]) -- even numbers
                      (ACBody (Call "*" [Var "x", Var "x"]))))) of
    Left e -> False
    Right v -> v == ArrayVal [IntVal 4]

testArrayScope :: Bool
testArrayScope =
  case runExpr (Comma (Assign "x" (Number 5))
               (Comma (Compr (ACFor "x" (Array [Number 1]) (ACBody (Var "x"))))
                      (Var "x"))) of
    Left e -> False
    Right x -> x == IntVal 5
