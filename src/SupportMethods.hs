module SupportMethods (randomPair) where

import System.Random

-- Função que gera um par de números inteiros aleatórios dentro dos limites fornecidos
randomPair :: Int -> Int -> IO Int
randomPair lowerBound upperBound = randomRIO (lowerBound, upperBound)
