module PlayerType (Player(..)) where

data Player = Player
  { 
    playerID :: Int,
    chainedDoubles :: Int, 
    carteira :: Int,
    deedsAssets :: [Int], --int Identifier
    isJailed :: Bool,
    numeroSairCadeia :: Int
  } | Bank deriving (Show)
