module Atividade04
    ( 
        next,
        timeList,
        soma,
        timeListRedl,
        bt,
        timeBT,
        timeT,
        redl,
        caminhosArvore,
        caminhosArvoreT,
        bestBT,
        bestT,
        tree,
        passarTempoNosSemaforos2,
        arratToList,
        listToArray,
        Sem(..),
        BT(..),
        List(..)
    ) where
import GHC.Exception (fromCallSiteList)
import Control.Concurrent (yield)
import Data.Char (toLower)
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

data Sem = Green | Yellow | Red deriving (Eq, Show)
data List a = Nil | a :- List a deriving (Show)
infixr 5 :-

next :: Sem -> Sem
next Green = Yellow
next Yellow = Red
next Red = Green

timeList :: List Sem -> Int
timeList Nil = 0
timeList (x:-xs) = tempoParaAtravessar x + timeList (passarTempoNosSemaforos xs (tempoParaAtravessar x )) 

passarTempoNosSemaforos :: List Sem -> Int -> List Sem
passarTempoNosSemaforos Nil _ = Nil
passarTempoNosSemaforos (x:-xs) 1 = next x :- passarTempoNosSemaforos xs 1
passarTempoNosSemaforos (x:-xs) 2 = next (next x) :- passarTempoNosSemaforos xs 2
passarTempoNosSemaforos (x:-xs) 3 = next (next (next x)) :- passarTempoNosSemaforos xs 3

passarTempoNosSemaforos2 :: Int -> Sem -> Int
passarTempoNosSemaforos2 acc sem = acc + tempoParaAtravessar sem

tempoParaAtravessar :: Sem -> Int
tempoParaAtravessar Green = 1
tempoParaAtravessar Yellow = 3
tempoParaAtravessar Red = 2

-- usei chat gpt
redl :: (b -> a -> b) -> b -> List a -> b
redl _ acc Nil = acc
redl f acc ( x :- xs) = redl f (f acc x) xs

soma :: List Int -> Int
soma xs = redl (+) 0 xs

timeListRedl :: List Sem -> Int
timeListRedl Nil = 0
timeListRedl (x:-xs) = redl passarTempoNosSemaforos2 (tempoParaAtravessar x ) (passarTempoNosSemaforos xs (tempoParaAtravessar x )) 

data BT a = BEmpty | Leaf a |BNode a (BT a) (BT a) deriving (Eq, Show)

bleaf :: a -> BT a
bleaf x = BNode x BEmpty BEmpty

bt :: BT Sem
bt = BNode Green
    (
        BNode Green (Leaf Red) (Leaf Green)
    )
    (
        BNode Yellow (Leaf Red) (Leaf Green)
    )

-- Usei Chat GPT
caminhosArvore :: BT a -> [[a]]
caminhosArvore (Leaf x) = [[x]]
caminhosArvore (BNode x left right) = map (x :) (caminhosArvore left) ++ map (x :) (caminhosArvore right)

caminhoDeMenorCusto :: [[Sem]] -> [Sem] -> Int -> ([Sem], Int)
caminhoDeMenorCusto [] lista custo = (lista, custo)
caminhoDeMenorCusto (x:xs) [] _ = caminhoDeMenorCusto xs x (timeList (arratToList x))
caminhoDeMenorCusto (x:xs) lista custo 
    | timeList (arratToList x) < custo = caminhoDeMenorCusto xs x (timeList (arratToList x))
    | otherwise = caminhoDeMenorCusto xs lista custo

timeBT :: BT Sem -> Int
timeBT tree =  snd (caminhoDeMenorCusto (caminhosArvore tree) [] (-1))

bestBT :: BT Sem -> [Sem]
bestBT tree = fst (caminhoDeMenorCusto (caminhosArvore tree) [] (-1))

data Tree a = TEmpty | TLeaf a| TNode a [Tree a] deriving Show

tree :: Tree Sem
tree = TNode Green [ TNode Green [TLeaf Red, TLeaf Green] , TNode Yellow [TNode Red [TLeaf Red, TLeaf Green, TLeaf Yellow]], TNode Green [TLeaf Green, TLeaf Green]]

caminhosArvoreT :: Tree a -> [[a]]
caminhosArvoreT TEmpty = []
caminhosArvoreT (TLeaf x) = [[x]]
caminhosArvoreT (TNode x children) = map (x :) (concatMap caminhosArvoreT children)

timeT :: Tree Sem -> Int
timeT tree =  snd (caminhoDeMenorCusto (caminhosArvoreT tree) [] (-1))

bestT :: Tree Sem -> [Sem]
bestT tree = fst (caminhoDeMenorCusto (caminhosArvoreT tree) [] (-1))

arratToList :: [a] -> List a
arratToList [] = Nil
arratToList (x:xs) = x :- arratToList xs

listToArray :: List a -> [a]
listToArray Nil = []
listToArray (x :- xs) = x : listToArray xs