module PlayerType (Player(..)) where

data Player = Player
  { numeroDadosIguais :: Int, 
    carteira :: Int, 
    numeroSairCadeia :: Int,
  } deriving (Show)
