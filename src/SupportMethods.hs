module SupportMethods (randomPair) where

import System.Random

-- Função que gera um par de números inteiros aleatórios dentro dos limites fornecidos
randomPair :: Int -> Int -> Pair (Int, Int)
randomPair lowerBound upperBound = do
  x <- randomRIO (lowerBound, upperBound)
  y <- randomRIO (lowerBound, upperBound)
  return (x, y)