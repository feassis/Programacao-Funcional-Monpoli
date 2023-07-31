module Eval where

data EvalTree a
  = Function (EvalTree a) a (EvalTree a)
  | Value a
  | NoOp
  | EvalError
  deriving (Show)

-- To simplify our life
data EvalTag x
  = Val x
  | Func x

toEvalTree :: Evaluable a => a -> EvalTree a
toEvalTree = tagToTree . toEvalTag
  where
    tagToTree :: EvalTag a -> EvalTree a
    tagToTree (Val a)  = Value a
    tagToTree (Func a) = Function NoOp a NoOp

class (Ord a) =>
      Evaluable a
  where
  applyFunction :: a -> a -> a -> Maybe a
  toEvalTag :: a -> EvalTag a

instance (Evaluable a) => Semigroup (EvalTree a) where
  NoOp <> x = x
  x <> NoOp = x
  EvalError <> _ = EvalError
  _ <> EvalError = EvalError
  a@(Function _ f1 _) <> (Function l2 f2 r2)
    | f1 > f2 = Function (a <> l2) f2 r2
  (Function l f r) <> x = Function l f (r <> x)
  x <> (Function l f r) = Function (x <> l) f r
  _ <> _ = EvalError

instance Evaluable a => Monoid (EvalTree a) where
  mempty = NoOp

runEvalTree :: Evaluable a => EvalTree a -> Maybe a
runEvalTree EvalError = Nothing
runEvalTree NoOp = Nothing
runEvalTree (Value x) = Just x
runEvalTree (Function l f r) = do
  ml <- runEvalTree l
  mr <- runEvalTree r
  applyFunction f ml mr

data IntExpr
  = Add
  | Mult
  | Sub
  | IntLit Int
  deriving (Show, Eq)

-- Should indicate precedence
instance Ord IntExpr where
  compare (IntLit _) _ = undefined -- should never happen
  compare _ (IntLit _) = undefined -- should never happen
  compare a b
    | a == b = EQ
  compare Mult _ = GT
  compare _ Mult = LT
  compare _ _ = EQ

instance Evaluable IntExpr where
  applyFunction Add (IntLit a) (IntLit b)  = Just $ IntLit (a + b)
  applyFunction Sub (IntLit a) (IntLit b)  = Just $ IntLit (a - b)
  applyFunction Mult (IntLit a) (IntLit b) = Just $ IntLit (a * b)
  applyFunction _ _ _                      = Nothing
  toEvalTag (IntLit x) = Val $ IntLit x
  toEvalTag f          = Func f

data BoolExpr
  = And
  | Or
  | BoolLit Bool
  deriving (Show, Eq)

instance Ord BoolExpr where
  compare _ (BoolLit _) = undefined -- should never happen
  compare (BoolLit _) _ = undefined -- should never happen
  compare And Or        = GT
  compare Or And        = LT
  compare _ _           = EQ

instance Evaluable BoolExpr where
  applyFunction And (BoolLit a) (BoolLit b) = Just $ BoolLit (a && b)
  applyFunction Or (BoolLit a) (BoolLit b)  = Just $ BoolLit (a || b)
  applyFunction _ _ _                       = Nothing
  toEvalTag (BoolLit x) = Val $ BoolLit x
  toEvalTag f           = Func f

evaluate :: (Evaluable a, Foldable t) => t a -> Maybe a
evaluate = runEvalTree . foldMap toEvalTree
