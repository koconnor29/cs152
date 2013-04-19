{-# LANGUAGE QuasiQuotes #-}

import Data.Map (Map)
import Data.Monoid
import qualified Data.Map as Map
import Data.SExp

-- The abstract syntax of a lambda language with printing
data Exp =
    Var String            -- x
  | Lambda String Exp     -- (lambda (x) e)
  | App Exp Exp           -- (e1 e2)
  | StringLit String      -- "foo"
  | Concat Exp Exp        -- (concat e1 e2)
  | Output Exp            -- (output e)
  | UnitLit               -- unit
  deriving (Eq, Ord, Show)

-- Values that expressions evaluate to
data Val =
    Closure String Exp Env
  | StringVal String
  | UnitVal
  deriving (Eq, Ord, Show)

-- The variable environment, mapping variables to values
type Env = Map String Val

-- ### BEGIN Part 1 ###

-- A monad which supports failure and output effects
-- (you should replace Undefined)
data MaybeWriter w a = MWriter (w -> Maybe (a, w))

-- ### END Part 1 ###

-- ### BEGIN Part 2 ###

mwReturn :: (Monoid w) => a -> MaybeWriter w a
mwReturn x = MWriter(\s -> Just (x, s))

mwBind :: (Monoid w) => MaybeWriter w a -> (a -> MaybeWriter w b) -> MaybeWriter w b
mwBind (MWriter mW) f = 
  MWriter (\s -> 
    case mW s of
      Nothing -> Nothing
      Just (v,s') -> 
        let (MWriter m') = f v
        in m' s' )

mwFailure :: (Monoid w) => MaybeWriter w a
mwFailure = MWriter(\s -> Nothing)

mwOutput :: (Monoid w) => w -> MaybeWriter w ()
mwOutput w = MWriter(\s -> Just ((), mappend s w) )

-- ### END Part 2 ###

instance (Monoid w) => Monad (MaybeWriter w) where
  return = mwReturn
  (>>=) = mwBind

-- ### BEGIN put your helpers here ###
coerceClo :: Val -> MaybeWriter String (String, Exp, Env)
coerceClo (Closure x e env) = return (x,e,env)
coerceClo _ = mwFailure 

coerceStr :: Val -> MaybeWriter String String
coerceStr (StringVal s) = return s
coerceStr _ = mwFailure

liftMaybe :: (Monoid w) => Maybe a -> MaybeWriter w a
liftMaybe Nothing = mwFailure
liftMaybe (Just x) = return x

-- ### END put your helpers here ###

-- ### BEGIN Part 3 ###

-- (evalM e env) evaluates 'e' in the environment 'env' to a final value inside
-- the MaybeWriter monad.  The monoid which the monad manipulates is a String,
-- which is the output that accumulates during evaluation.
evalM :: Exp -> Env -> MaybeWriter String Val
evalM (Var x) env = liftMaybe (Map.lookup x env) 
evalM (Lambda x e) env = return (Closure x e env)
evalM (App e1 e2) env = do
  v1 <- evalM e1 env
  (x,body,env') <- coerceClo v1
  v2 <- evalM e2 env
  v3 <- evalM body (Map.insert x v2 env')
  return v3
evalM (StringLit s) env = return (StringVal s)
evalM (Concat e1 e2) env = do
  s1 <- coerceStr =<< evalM e1 env 
  s2 <- coerceStr =<< evalM e2 env
  return (StringVal(s1 ++ s2))
evalM (Output e) env = do
  s <- coerceStr =<< evalM e env
  (\u -> return UnitVal) =<< mwOutput s 
evalM (UnitLit) env = return (UnitVal)
-- ### END Part 3 ###

-- ### BEGIN Part 4 ###

-- (eval e env) evaluates 'e' in the environment 'env' to a final pure result.
--
-- The result is either Nothing, indicating failure, or Just (v, out)
-- indicating a result value 'v' with global output 'out'.
eval :: Exp -> Env -> Maybe (Val, String)
eval exp env = 
  let MWriter(mW) = evalM exp env in
  mW ""

-- ### END Part 4 ###

-- A few simple examples.
-- You should make lots of your own examples!

-- finishes with the value `unit` and the string "oh hai"
e1 :: Exp
e1 = parse [sexp|

((lambda (z)
  (output " hai"))
 (output "oh"))

|]

-- finishes with the value `unit` and the string "oh hai"
e2 :: Exp
e2 = parse [sexp| 

((lambda (fconcat)
 (output ((((lambda (x) x) 
            fconcat) 
           "oh") 
          " hai")))
 (lambda (x) 
  (lambda (y) 
   ((lambda (z) 
     (concat x y))
    unit))))

|]

e3 :: Exp
e3 = parse [sexp|
  (lambda (z) z)

|]

e4 :: Exp
e4 = parse [sexp|
  (concat "HEY " "YOU")
|]

e5 :: Exp
e5 = parse [sexp|
  ((lambda (x) (concat "Hey " x)) "Teachers, Leave Those Kids Alone")
|]

e6 :: Exp
e6 = parse [sexp|
  (((lambda (f) (lambda (x) (f x))) (lambda (x) (concat "Hey" x))) "Kevin")

|]

e7 :: Exp
e7 = parse [sexp|
  ((lambda (x) x) "Kevin")
|]

e8 :: Exp
e8 = parse [sexp|
  (output ((lambda (x) (x "MAN")) ((lambda (x) (lambda (y) (concat x y))) "SUPER")))
|]

-- The main function that is called when you 'runhaskell eval.hs'.
-- As is, it prints "oh hai"
--
-- You are welcome to change the main function all you like.  We will not use
-- it in grading.
-- main :: IO ()
-- main = putStrLn "oh hai"

-- This commented out main function will run e1 through your evaluator and
-- print the output.
main :: IO ()
main = putStrLn $ show $ eval e8 Map.empty

---------------------------------------------------
----- parsing s-expressions into our language -----
----- don't worry about understanding this --------
---------------------------------------------------
parse :: SExp -> Exp
parse   [sexp| unit |] = UnitLit
parse s@[sexp| lambda |] = error $ "bad 'lambda' expression: " ++ show s
parse s@[sexp| concat |] = error $ "bad 'concat' expression: " ++ show s
parse s@[sexp| output |] = error $ "bad 'output' expression: " ++ show s
parse   [sexp| @str:s |] = StringLit s
parse   [sexp| @sym:x |] = Var x
parse   [sexp| (lambda (@sym:x) @:e) |] = Lambda x (parse e)
parse s@[sexp| (lambda . @:_) |] = error $ "bad 'lambda' expression: " ++ show s
parse   [sexp| (concat @:e1 @:e2) |] = Concat (parse e1) (parse e2)
parse s@[sexp| (concat . @:_) =|] = error $ "bad 'concat' expression: " ++ show s
parse   [sexp| (output @:e) |] = Output (parse e)
parse s@[sexp| (output . @:_) |] = error $ "bad 'output' expression: " ++ show s
parse   [sexp| (@:e1 @:e2) |] = App (parse e1) (parse e2)
parse _ = error "could not parse s-expression into Exp"
---------------------------------------------------
