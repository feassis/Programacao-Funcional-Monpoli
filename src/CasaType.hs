module CasaType (Casa(..)) where

data Casa = Casa
  { dono :: [Char],
    aluguel :: [Int],
    level :: Int,
    cinsequenciaId :: Int
  } deriving (Show)