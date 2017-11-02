{-
Example code produced for Advanced Programming lecture.

Main purpose is to show how to generate recursive types.
-}
import Test.QuickCheck
import Control.Monad (liftM2)
import Control.Applicative

data Expr = Con Int
          | Add Expr Expr
          | Minus Expr Expr
     deriving (Eq, Show, Read, Ord)

eval :: Expr -> Int
eval (Con n) = n
eval (Add x y) = eval x + eval y
eval (Minus x y) = eval x - eval y

prop_com_add x y = eval (Add x y) == eval (Add y x)

--Take 1: not limiting the recursion
expr =  oneof [ fmap Con arbitrary
              , do x <- expr
                   y <- expr
                   return $ Add x y
              ]

-- -- Take 2: using sized generators
-- instance Arbitrary Expr where
--   arbitrary = expr2

expr2 = sized exprN
exprN 0 = fmap Con arbitrary
exprN n = oneof [ fmap Con arbitrary
                , liftM2 Add subexpr subexpr
                , liftM2 Minus subexpr subexpr
                ]
  where subexpr = exprN (n `div` 2)


instance Arbitrary Expr where
  arbitrary = expr2

  -- For extra nice counter examples define a shink funktion
  shrink (Add e1 e2) = [e1, e2]
  shrink (Minus e1 e2) = [e1, e2]
  shrink _ = []


-- Checking that minus is commutative
-- and use `counterexample` to print a nice explanation when the
-- property fails
prop_com_minus x y =
  counterexample (show exp1 ++ " is not equal to "++ show exp2) $
  eval exp1 == eval exp2
  where exp1 = Minus x y
        exp2 = Minus y x
