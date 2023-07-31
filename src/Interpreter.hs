module Interpreter (run, intToChurch, churchToInt, runInt, churchToBool, runBool) where

import Definitions
import Data.Char (isDigit)
import Data.List (union)

eval :: Env -> LambdaExpr -> LambdaExpr
eval env (Apply m e) =
  case eval env m of
    Lambda n b -> eval env $ betaRed env n e b
    _       -> Apply m e
eval env (Var n) = maybe (Var n) (eval env) (lookup n env)
eval _ e = e

betaRed :: Env -> Name -> LambdaExpr -> LambdaExpr -> LambdaExpr
betaRed _ redVar expr (Var s)
  | s == redVar = expr
  | otherwise   = Var s
betaRed env redVar expr (Lambda s m)
  | s == redVar  = Lambda s m
  | s `elem` fvs = Lambda s1 $ rec $ renameVar s s1 m
  | otherwise    = Lambda s (rec m)
  where
    fvs = fv env expr
    s1 = newName s $ redVar : fvs `union` fv env m
    rec = betaRed env redVar expr
betaRed env redVar expr (Apply m n) =
  Apply (rec m) (rec n)
  where
    rec = betaRed env redVar expr

-- Considerando o Environemnt devolva a lista de variáveis livres
fv :: Env -> LambdaExpr -> [Name]
fv =
  fv' []
  where
    fv' :: [Name] -> Env -> LambdaExpr -> [Name]
    fv' vs env (Var s)
      | s `elem` vs = []
      -- Trata lets recursivos
      | Just x <- lookup s env = fv' (s:vs) env x
      | otherwise              = [s]
    fv' vs env (Apply x y) = fv' vs env x `union` fv' vs env y
    fv' vs env (Lambda s b)   = fv' (s:vs) env b

-- Devolve um novo nome de variável (x) que não esteja em uso (ys)
newName :: Name -> [Name] -> Name
newName x ys =
  head $ filter (`notElem` ys) candNames
  where
    baseName  = takeWhile (not . isDigit) x
    candNames = map ((baseName ++) . show) ([1..] :: [Int])


renameVar :: Name -> Name -> LambdaExpr -> LambdaExpr
renameVar from to (Var s)
  | s == from = Var to
  | otherwise = Var s
renameVar from to fn@(Lambda s b)
  | s == from = fn
  | otherwise = Lambda s (renameVar from to b)
renameVar from to (Apply fn b) =
  Apply (rec fn) (rec b)
  where
    rec = renameVar from to

nf :: Env -> LambdaExpr -> LambdaExpr
nf env expr =
  case eval env expr of
    Var n   -> Var n
    Lambda n m -> Lambda n (nf env m)
    Apply m n -> Apply (nf env m) (nf env n)

-- Recebe uma expressão lambda e executa (isso é, deixa-a na forma normal)
run :: LambdaExpr -> LambdaExpr
run = nf []

---------------------------------------------------
---------------------------------------------------
-- Lambdações para facilitar o uso do interpretador --
---------------------------------------------------
---------------------------------------------------

intToChurch :: Int -> LambdaExpr
intToChurch n =
  run $ intToChurch' n
  where
    intToChurch' 0 = zero
    intToChurch' m = Apply increment (intToChurch (m - 1))

churchToInt :: LambdaExpr -> Int
churchToInt (Lambda _ (Lambda _ body)) =
  countFs 0 $ run body
  where countFs c (Var _)     = c
        countFs c (Apply _ b) = countFs (c+1) b
        countFs _ _           = undefined -- Mal formado ou não normalizado
churchToInt _ = undefined -- idem acima

churchToBool :: LambdaExpr -> Bool
churchToBool e = enf == true
  where
    enf = run $ Apply churchBool e

runInt :: LambdaExpr -> Int
runInt = churchToInt . run

runBool:: LambdaExpr -> Bool
runBool = churchToBool . run