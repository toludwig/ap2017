module SubsInterpreter
       (
         Value(..)
       , runExpr
       , assocTest
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad (mapM)
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(intercalate)
import Test.QuickCheck (quickCheck)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

-- Fills initialContext with empty variable environment and a procedure environment with basic operations

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", strictEquals)
                       , ("<", lessThan)
                       , ("+", plus)
                       , ("*", mult)
                       , ("-", sub)
                       , ("%", modulus)
                       , ("Array", mkArray)
                       ]

-- Primitive implementation of ===

strictEquals :: Primitive
strictEquals [IntVal i1,IntVal i2] | i1 == i2          = Right TrueVal
strictEquals [UndefinedVal,_]                          = Right FalseVal
strictEquals [TrueVal,TrueVal]                         = Right TrueVal
strictEquals [FalseVal,FalseVal]                       = Right TrueVal
strictEquals [StringVal s1,StringVal s2] | s1 == s2    = Right TrueVal
strictEquals [ArrayVal vs1, ArrayVal vs2] | vs1 == vs2 = Right TrueVal
strictEquals [_,_]                                     = Right FalseVal
strictEquals _                                         = Left "=== called with wrong number of arguments"

-- Primitive implementation of <

lessThan :: Primitive
lessThan [IntVal i1,IntVal i2] | i1 < i2       = Right TrueVal
lessThan [StringVal s1,StringVal s2] | s1 < s2 = Right TrueVal
lessThan [_,_]                                 = Right FalseVal
lessThan _                                     = Left "< called with incorrect number of arguments"

-- Primitive implementation of +

plus :: Primitive
plus [IntVal i1, IntVal i2]      = Right (IntVal (i1+i2))
plus [StringVal s1,StringVal s2] = Right (StringVal (s1++s2))
plus [StringVal s,IntVal i]      = Right (StringVal (s ++ show i))
plus [IntVal i, StringVal s]     = Right (StringVal (show i ++ s))
plus [_,_]                       = Left "Only Strings and Integers may be added"
plus _                           = Left "+ called with incorrect number of arguments"

-- Primitive implementation of *

mult :: Primitive
mult [IntVal a, IntVal b] = Right (IntVal (a*b))
mult [_, _]              = Left "Only Integers may be multiplied"
mult _                   = Left "* called with the wrong number of arguments"

-- Primitive implementation of -

sub :: Primitive
sub [IntVal i1, IntVal i2] = Right (IntVal (i1-i2))
sub [_,_]                  = Left "Only Integers may be subtracted"
sub _                      = Left "- called with incorrect number of arguments"

-- Primitive implementation of %

modulus :: Primitive
modulus [IntVal a, IntVal b] = Right (IntVal (a `mod` b))
modulus [_, _]               = Left "only Integers may be taken %"
modulus _                    = Left "wrong number of arguments applied to %"

-- Primitive implementation of Array(n)

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"



newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

-- Implementation of fmap function with SubsM functor

instance Functor SubsM where
  fmap f fra = SubsM (\c -> case runSubsM fra c of
    (Left e)        -> Left e
    (Right (a,env)) -> Right (f a,env))

-- Implmementation of <*> and pure functions to SubsM Applicative functor

instance Applicative SubsM where
  pure = return
  ff <*> fa = SubsM (\c0 -> case runSubsM ff c0 of
    (Left e)         -> Left e
    (Right (f, env)) -> case runSubsM fa (env,snd c0) of
      (Left e')         -> Left e'
      (Right (a, env')) -> Right (f a, env'))

-- Implementation of <<=, return and fail for SubsM Monad

instance Monad SubsM where
  return x = SubsM (\c -> Right (x, fst c))
  m >>= f  = SubsM (\c0 -> case runSubsM m c0 of
     (Left e)        -> Left e
     (Right (x,env)) -> runSubsM (f x) (env,snd c0))
  fail s = SubsM (\_ -> Left s)

-- Function to update old variable environment with a new one

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\c -> Right ((),f (fst c)))

-- Function to create a new variable environment with updated variable and update the context

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv (Map.insert name val)

-- Function to retrieve the value of a variable from the current context

getVar :: Ident -> SubsM Value
getVar name = SubsM (\c -> case Map.lookup name (fst c) of
  Nothing -> Left "Variable not initialised"
  (Just x)  -> Right (x,fst c))

-- Function to retrieve a primitive operation from the current context

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\c -> case Map.lookup name (snd c) of
  Nothing  -> Left "Function name not initialised"
  (Just x) -> Right (x,fst c))

-- function that takes an expression and evaluates it using a context
evalExpr :: Expr -> SubsM Value
-- Evaluates a basic expression, doesn't change the context and returns the correct value
evalExpr (Number x)  = SubsM (\c -> Right (IntVal x,fst c))
evalExpr (String s)  = SubsM (\c -> Right (StringVal s,fst c))
evalExpr Undefined   = SubsM (\c -> Right (UndefinedVal,fst c))
evalExpr TrueConst   = SubsM (\c -> Right (TrueVal,fst c))
evalExpr FalseConst  = SubsM (\c -> Right (FalseVal,fst c))
-- Evaluates arrays as ArrayVals
evalExpr (Array exprs) = do
  vals <- mapM evalExpr exprs
  return (ArrayVal vals)
evalExpr (Var name)  = getVar name
-- Evaluates comprehensions keeping the original context
evalExpr (Compr aComp) = SubsM (\c -> case runSubsM m c of
  Left e    -> Left e
  Right res -> Right (fst res,fst c)) where
    m = evalCompr aComp
-- Evaluates the function call expression
evalExpr (Call name exprs) = SubsM (\c -> case runSubsM (mapM evalExpr exprs) c of
  Left e          -> Left e
  Right (vals,env) -> case runSubsM (getFunction name) (env,snd c) of
    Left e           -> Left e
    Right (prim,env') -> case prim vals of
      Left e    -> Left e
      Right val -> Right (val,env'))

evalExpr (Assign name expr) = do
   val <- evalExpr expr
   putVar name val
   return val

evalExpr (Comma expr1 expr2) = do
  _ <- evalExpr expr1
  evalExpr expr2

evalCompr :: ArrayCompr -> SubsM Value
evalCompr (ACBody expr)                     = evalExpr expr

evalCompr (ACFor name (String string) comp) = do
  vals <- mapM iterS string
  return (ArrayVal vals) where
    iterS x = do
        val <- evalExpr (String [x])
        putVar name val
        evalCompr comp
evalCompr (ACFor name expr comp) = do
  (ArrayVal array) <- evalExpr expr
  vals <- mapM iterAr array
  return (ArrayVal vals) where
    iterAr x = do
      putVar name x
      evalCompr comp

evalCompr (ACIf expr comp) = do
  bool <- evalExpr expr
  if checkBool bool == 1
    then evalCompr comp
    else if checkBool bool == 2
      then return (ArrayVal [])
      else SubsM (const (Left "If must be supplied with a boolean expression"))

checkBool :: Value -> Int
checkBool TrueVal  = 1
checkBool FalseVal = 2
checkBool _        = 3

runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
  Left e          -> Left e
  Right res -> Right (fst res)

succSubsM :: Int -> SubsM Int
succSubsM n = SubsM (\c -> Right (n+1,fst c))

squareSubsM :: Int -> SubsM Int
squareSubsM n = SubsM (\c -> Right (n*n,fst c))

mult10SubsM :: Int -> SubsM Int
mult10SubsM n = SubsM (\c -> Right (n*10,fst c))

assocEx1 :: Int -> SubsM Int
assocEx1 n = do
  y <- do
    x <- succSubsM n
    squareSubsM x
  mult10SubsM y

assocEx2 :: Int -> SubsM Int
assocEx2 n = do
  x <- succSubsM n
  y <- squareSubsM x
  mult10SubsM y

property_assoc_test :: Int -> Bool
property_assoc_test n = (runSubsM (assocEx1 n) initialContext) == (runSubsM (assocEx2 n) initialContext)

assocTest :: IO ()
assocTest = quickCheck property_assoc_test
