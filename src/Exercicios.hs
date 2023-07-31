module Exercicios where

import qualified Data.List   as L
import           Definitions
import           Interpreter
import           Eval

------------
-- TEMA 1 --
------------

-- MONOIDE
data Resultado
  = Pontuacao Int
  | Cola
  deriving (Show, Eq)

data Set a =
  Set [a]
  deriving (Eq)

fromList :: Ord a => [a] -> Set a
fromList = undefined

member :: Ord a => a -> Set a -> Bool
member = undefined

insert :: Ord a => a -> Set a -> Set a
insert = undefined

delete :: Ord a => a -> Set a -> Set a
delete = undefined

data Dieta
  = Vegano
  | Vegetariano
  | Tradicional
  deriving (Show, Eq)

data Lanche =
  Lanche (Set String) Int Dieta
  deriving (Show, Eq)

------------
-- TEMA 2 --
------------

-- LAMBDA
null_, nil, cons, head_, tail_ :: LambdaExpr
null_ = Lambda "l" $
  apply2 (Var "l") (Lambda "h" $ Lambda "t" $ Lambda "d" false) true

nil = undefined

cons = undefined

head_ = undefined

tail_ = undefined

listOfIntToChurch :: [Int] -> LambdaExpr
listOfIntToChurch = undefined

runListOfInt :: LambdaExpr -> [Int]
runListOfInt = undefined

at :: LambdaExpr
at = undefined

filter_ :: LambdaExpr
filter_ = undefined

------------
-- TEMA 3 --
------------

newtype Parser a =
  Parser
    { runParser :: String -> Maybe (String, a)
    }

-- | Basic building block: parsing a character
charP :: Char -> Parser Char
charP c = Parser p
  where
    p (x:xs)
      | x == c = Just (xs, x)
    p _ = Nothing

---------------------
-- TODO: instances --
---------------------
-- 1. Functor
-- 2. Applicative
-- 3. Monad
---------------------

-- | Receives a list of parsers and tries them in order, returning the result of the first one that succeeds
-- $> runParser (firstOf [charP '0', charP '1', charP '2']) "1"
-- Just ("",'1')
firstOfP :: [Parser a] -> Parser a
firstOfP parsers = Parser $ tryFirst parsers
  where
    tryFirst (p:ps) xs =
      case runParser p xs of
        Nothing       -> tryFirst ps xs
        justSomething -> justSomething
    tryFirst _ _ = Nothing

------------------------
-- TODO: more helpers --
------------------------
oneOrMoreP :: Parser a -> Parser [a]
oneOrMoreP parser = undefined

optionalP :: Parser a -> Parser (Maybe a)
optionalP parser = undefined

-- | Helper to force a parser to consume the entire string, or fail
parse :: Parser a -> String -> Maybe a
parse p x =
  case runParser p x of
    Just ("", result) -> Just result
    _                 -> Nothing

--------------------------------
-- Yay, more complex parsers! --
--------------------------------
--
-- Digits and ints
digitP :: Parser Char
digitP = firstOfP $ map charP "0123456789"

intP :: Parser Int
intP = read <$> (oneOrMoreP digitP)

-- Int Expressions
addP, subP, multP, intLitP, intExprP :: Parser IntExpr
addP = const Add <$> charP '+'

subP = const Sub <$> charP '-'

multP = const Mult <$> charP '*'

intLitP = IntLit <$> intP

intExprP = firstOfP [intLitP, addP, subP, multP]

-- | Parses many IntExpr
-- $> parse listOfIntExprP "5+3*7-1" >>= evaluate
-- Just (IntLit 25)
listOfIntExprP :: Parser [IntExpr]
listOfIntExprP = oneOrMoreP intExprP

-- | Parses many BoolExpr
listOfBoolExprP :: Parser [BoolExpr]
listOfBoolExprP = undefined

--
-- Text
-- | Parses any lowercase english alphabet character
lowercaseP :: Parser Char
lowercaseP = firstOfP $ map charP ['a' .. 'z']

-- | Parses any uppercase english alphabet character
uppercaseP :: Parser Char
uppercaseP = firstOfP $ map charP ['A' .. 'Z']

-- | Parses an exact string
-- $> runParser (stringP "bat") "batman"
-- Just ("man","bat")
stringP :: String -> Parser String
stringP = sequenceA . (map charP)

alphaP :: Parser Char
alphaP = firstOfP [lowercaseP, uppercaseP]

-- | Parses any string made of alphabetic characters
alphaStringP :: Parser String
alphaStringP = oneOrMoreP alphaP

-- Names
-- | Parses just the first and last name
-- $> parse firstLastNameP "Haskell Curry"
-- Just ("Haskell","Curry")
firstLastNameP :: Parser (String, String)
firstLastNameP = do
  firstName <- alphaStringP
  _ <- oneOrMoreP $ charP ' '
  lastName <- alphaStringP
  return $ (firstName, lastName)

-- | Parses a really long name, dividing it into words
-- $> parse longNameP "Pedro de Alcantara Joao Carlos Leopoldo Salvador Bibiano Francisco Xavier de Paula Leocadio Miguel Gabriel Rafael Gonzaga de Habsburgo Lorena e Braganca"
-- Just ["Pedro","de","Alcantara","Joao","Carlos","Leopoldo","Salvador","Bibiano","Francisco","Xavier","de","Paula","Leocadio","Miguel","Gabriel","Rafael","Gonzaga","de","Habsburgo","Lorena","e","Braganca"]
longNameP :: Parser [String]
longNameP =
  oneOrMoreP $ do
    name <- alphaStringP
    _ <- optionalP $ oneOrMoreP $ charP ' '
    return name

-- Alpha Numeric Text
-- | Parses a single alphanumeric char
alphaNumP :: Parser Char
alphaNumP = firstOfP [alphaP, digitP]

-- | Parses an alphanumeric string
alphaNumStrP :: Parser String
alphaNumStrP = oneOrMoreP alphaNumP

-- YAML!
-- | Parses a single line of yaml, i.e., a key-value pair
yamlLineP :: Parser (String, String)
yamlLineP = do
  key <- alphaNumStrP
  _ <- stringP ": "
  val <- alphaNumStrP
  return (key, val)

-- | Parses several lines of yaml.
-- The last line may omit the closing "\n"
-- $> parse yamlP "name: parserlib\nversion: 1\nbranch: master"
-- Just [("name","parserlib"),("version","1"),("branch","master")]
yamlP :: Parser [(String, String)]
yamlP = oneOrMoreP $ yamlLineP <* optionalP (stringP "\n")
