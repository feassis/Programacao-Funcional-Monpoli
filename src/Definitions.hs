module Definitions where

type Name = String

type Env = [(Name, LambdaExpr)]

data LambdaExpr
  = Var Name
  | Lambda Name LambdaExpr
  | Apply LambdaExpr LambdaExpr
  deriving (Eq, Show)

-- MÉTODOS AUXILIARES
-- Aplica a função `f` (com 2 parâmetros) a `x` e `y`
apply2 :: LambdaExpr -> LambdaExpr -> LambdaExpr -> LambdaExpr
apply2 f x y = Apply (Apply f x) y

-- Aplica a função `f` (com 3 parâmetros) a `x`, `y` e `z`
apply3 :: LambdaExpr -> LambdaExpr -> LambdaExpr -> LambdaExpr -> LambdaExpr
apply3 f x y z = Apply (apply2 f x y) z

-- Use esse método para mostrar uma expressão em tela pra ajudar no debug
-- Exemplo: putStrLn $ prettyShow identity
prettyShow :: LambdaExpr -> String
prettyShow (Var n) = n
prettyShow (Lambda fn l) = "λ" ++ fn ++ " -> " ++ prettyShow l
prettyShow (Apply (Var x) (Var y)) = x ++ " " ++ y
prettyShow (Apply x (Var y)) = "(" ++ prettyShow x ++ ") " ++ y
prettyShow (Apply (Var x) y) = x ++ " " ++ "(" ++ prettyShow y ++ ")"
prettyShow (Apply x y) =
  "(" ++ prettyShow x ++ ") " ++ "(" ++ prettyShow y ++ ")"

identity :: LambdaExpr
identity = Lambda "x" (Var "x")

---------------
-- Booleanos --
---------------
true, false, if_ :: LambdaExpr
true = Lambda "x" $ Lambda "y" $ Var "x"

false = Lambda "x" $ Lambda "y" $ Var "y"

if_ =
  Lambda "b" $ Lambda "x" $ Lambda "y" $ apply2 (Var "b") (Var "x") (Var "y")

not_, and_, or_, xor :: LambdaExpr
not_ = Lambda "b" $ apply2 (Var "b") false true

and_ = Lambda "b" $ Lambda "c" $ apply2 (Var "b") (Var "c") false

or_ = Lambda "b" $ Lambda "c" $ apply2 (Var "b") true (Var "c")

xor =
  Lambda "m" $ Lambda "n" $ apply2 (Var "m") (Apply not_ $ Var "n") (Var "n")

churchBool :: LambdaExpr
churchBool =
  Lambda "x" $ -- Util para simplificar/normalizar expressoes booleanas
  apply3 if_ (Var "x") true false

--------------
-- Inteiros --
--------------
zero, one :: LambdaExpr
zero = Lambda "f" $ Lambda "x" (Var "x")

one = Lambda "f" $ Lambda "x" $ Apply (Var "f") (Var "x")

two = Lambda "f" $ Lambda "x" $ Apply (Var "f") $ Apply (Var "f") (Var "x")

five =
  Lambda "f" $
  Lambda "x" $
  Apply (Var "f") $
  Apply (Var "f") $
  Apply (Var "f") $ Apply (Var "f") $ Apply (Var "f") (Var "x")

-- Aritmética de inteiros
increment, add, decrement, mul, exp_ :: LambdaExpr
increment =
  Lambda "n" $
  Lambda "f" $
  Lambda "x" $ Apply (Var "f") $ Apply (Apply (Var "n") (Var "f")) (Var "x")

add = Lambda "n" $ Lambda "b" $ apply2 (Var "n") increment (Var "b")

decrement =
  Lambda "n" $
  Lambda "f" $
  Lambda "x" $
  Apply
    (Apply
       (Apply
          (Var "n")
          (Lambda "g" $ Lambda "h" (Apply (Var "h") (Apply (Var "g") (Var "f")))))
       (Lambda "u" (Var "x")))
    identity

mul =
  Lambda "m" $
  Lambda "n" $ Lambda "f" $ Apply (Var "m") (Apply (Var "n") (Var "f"))

exp_ = Lambda "m" $ Lambda "n" $ Apply (Var "n") (Var "m")

sub = Lambda "m" $ Lambda "n" $ apply2 (Var "n") decrement (Var "m")

-- Comparação de inteiros
isZero, churchEqual, churchDiff :: LambdaExpr
isZero = Lambda "n" $ Apply (Apply (Var "n") (Lambda "x" false)) true

churchEqual =
  Lambda "x" $
  Lambda "y" $
  Apply churchBool $
  apply2
    and_
    (Apply isZero $ apply2 sub (Var "x") (Var "y")) -- Pois se x < y, x - y == 0
    (Apply isZero $ apply2 sub (Var "y") (Var "x"))

churchDiff =
  Lambda "x" $
  Lambda "y" $
  Apply churchBool $ Apply not_ $ apply2 churchEqual (Var "x") (Var "y")

churchLess =
  Lambda "x" $
  Lambda "y" $
  Apply churchBool $
  apply2
    and_
    (apply2 churchDiff (Var "x") (Var "y")) -- nao é igual
    (Apply isZero (apply2 sub (Var "x") (Var "y"))) -- sub == 0, entao x < y

------------
-- Tuplas --
------------
pair, fst_, snd_ :: LambdaExpr
pair =
  Lambda "f" $ Lambda "s" $ Lambda "b" $ apply2 (Var "b") (Var "f") (Var "s")

fst_ = Lambda "p" $ Apply (Var "p") true

snd_ = Lambda "p" $ Apply (Var "p") false

--------------
-- Recursão --
--------------
yComb :: LambdaExpr
yComb = Lambda "f" $ Apply part part
  where
    part = Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x")))

factorial :: LambdaExpr
factorial =
  Lambda "f" $
  Lambda "n" $
  -- a aplicação do if é desnecessária (veja pq na aula!) compare a
  -- versão impressa de `factorial` no GHCi com a versão `run factorial`
  apply3
    if_
    (Apply isZero $ Var "n")
    one -- then
    (apply2 mul (Var "n") (Apply (Var "f") (Apply decrement $ Var "n"))) -- else

-- Para executar: runInt $ apply2 yComb fib three
fib :: LambdaExpr
fib =
  Lambda "f" $
  Lambda "n" $
  apply3 if_ (apply2 churchLess (Var "n") (Apply increment one)) one $ -- then
        -- else:
  apply2
    add
    (Apply (Var "f") (Apply decrement $ Var "n"))
    (Apply (Var "f") (Apply decrement $ Apply decrement $ Var "n"))
