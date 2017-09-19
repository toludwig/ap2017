module SubsInterpreter
       (
         Value(..)
       , runExpr
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


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

strictEquals :: Primitive
strictEquals [IntVal i1,IntVal i2] | i1 == i2          = Right TrueVal
strictEquals [UndefinedVal,_]                          = Right FalseVal
strictEquals [TrueVal,TrueVal]                         = Right TrueVal
strictEquals [FalseVal,FalseVal]                       = Right TrueVal
strictEquals [StringVal s1,StringVal s2] | s1 == s2    = Right TrueVal
strictEquals [ArrayVal vs1, ArrayVal vs2] | vs1 == vs2 = Right TrueVal
strictEquals [_,_]                                     = Right FalseVal
strictEquals _                                         = Left "=== called with wrong number of arguments"

lessThan :: Primitive
lessThan [IntVal i1,IntVal i2] | i1 < i2       = Right TrueVal
lessThan [StringVal s1,StringVal s2] | s1 < s2 = Right TrueVal
lessThan [_,_]                                 = Right FalseVal
lessThan _                                     = Left "< called with incorrect number of arguments"

plus :: Primitive
plus [IntVal i1, IntVal i2]      = Right (IntVal (i1+i2))
plus [StringVal s1,StringVal s2] = Right (StringVal (s1++s2))
plus [StringVal s,IntVal i]      = Right (StringVal (s ++ (show i)))
plus [IntVal i, StringVal s]     = Right (StringVal ((show i) ++ s))
plus [_,_]                       = Left "Only Strings and Integers may be added"
plus _                           = Left "+ called with incorrect number of arguments"

mult :: Primitive
mult [IntVal a, IntVal b] = Right (IntVal (a*b))
mult [_, _]              = Left "Only Integers may be multiplied"
mult _                   = Left "* called with the wrong number of arguments"

sub :: Primitive
sub [IntVal i1, IntVal i2] = Right (IntVal (i1-i2))
sub [_,_]                  = Left "Only Integers may be subtracted"
sub _                      = Left "- called with incorrect number of arguments"

modulus :: Primitive
modulus [IntVal a, IntVal b] = Right (IntVal (a `mod` b))
modulus [_, _]               = Left "only Integers may be taken %"
modulus _                    = Left "wrong number of arguments applied to %"

mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"



newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap f fra = SubsM (\c -> case runSubsM fra c of
    (Left e)        -> Left e
    (Right (a,env)) -> Right (f a,env))

instance Applicative SubsM where
  pure a = return a
  ff <*> fa = SubsM (\c0 -> case runSubsM ff c0 of
    (Left e)         -> Left e
    (Right (f, env)) -> case runSubsM fa (env,snd c0) of
      (Left e')         -> Left e'
      (Right (a, env')) -> Right (f a, env'))

instance Monad SubsM where
  return x = SubsM (\c -> Right (x, fst c))
  m >>= f  = SubsM (\c0 -> case runSubsM m c0 of
     (Left e)        -> Left e
     (Right (x,env)) -> runSubsM (f x) (env,snd c0))
  fail s = SubsM (\c -> (Left s))

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM (\c -> Right ((),f (fst c)))

putVar :: Ident -> Value -> SubsM ()
putVar name val = modifyEnv (Map.insert name val)

getVar :: Ident -> SubsM Value
getVar name = SubsM (\c -> case Map.lookup name (fst c) of
  Nothing -> Left "Variable not initialised"
  (Just x)  -> Right (x,fst c))

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM (\c -> case Map.lookup name (snd c) of
  Nothing  -> Left "Function name not initialised"
  (Just x) -> Right (x,fst c))

evalExpr :: Expr -> SubsM Value
evalExpr (Number x)  = SubsM (\c -> (Right ((IntVal x),fst c)))
evalExpr (String s)  = SubsM (\c -> (Right ((StringVal s),fst c)))
evalExpr (Array exprs) = (mapM evalExpr exprs) >>= f where
  f vals = SubsM (\c -> (Right ((ArrayVal vals),fst c)))
evalExpr Undefined   = SubsM (\c -> (Right (UndefinedVal,fst c)))
evalExpr TrueConst   = SubsM (\c -> (Right (TrueVal,fst c)))
evalExpr FalseConst  = SubsM (\c -> (Right (FalseVal,fst c)))
evalExpr (Var name)  = getVar name
evalExpr (Compr aComp) = do
  val <- evalCompr aComp
  return val
evalExpr (Call name exprs) = SubsM (\c -> case runSubsM (mapM evalExpr exprs) c of
  Left e          -> Left e
  Right (vals,env) -> case runSubsM (getFunction name) (env,snd c) of
    Left e           -> Left e
    Right (prim,env) -> case prim vals of
      Left e    -> Left e
      Right val -> Right (val,env))
evalExpr (Assign name expr) = do
   val <- (evalExpr expr)
   putVar name val
   return val

--evalExpr (Assign name expr) = (evalExpr expr) >>= (putVar name) >> (evalExpr expr)

evalExpr (Comma expr1 expr2) = do
  evalExpr expr1
  evalExpr expr2

evalCompr :: ArrayCompr -> SubsM Value
evalCompr (ACBody expr)           = evalExpr expr
evalCompr (ACFor name (Array exprs) (ACBody expr')) = (mapM f exprs) >>= g where
  f expr = do
    val <- evalExpr expr
    putVar name val
    evalCompr (ACBody expr')
  g vals = SubsM (\c -> (Right ((ArrayVal vals),fst c)))
evalCompr _ = SubsM (\c -> (Left "Only expressions or one for comprehension permitted at the moment"))



runExpr :: Expr -> Either Error Value
runExpr expr = case runSubsM (evalExpr expr) initialContext of
  Left e          -> (Left e)
  Right (val,env) -> (Right val)
