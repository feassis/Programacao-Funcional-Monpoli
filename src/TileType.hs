module TileType (Land(..)) where

import PlayerType

data PieceSet = Brown | LightBlue | Pink | Orange | Red | Yellow | Green | DarkBlue | UT | RR

class Tile a where
  tileEvent :: a -> Player -> [Player] -> [Player]
  identifier :: a -> Int --retorna posicao no tabuleiro

class Tile a => Deed a where
  owner :: a-> Int
  setClass :: a-> PieceSet
  trade :: a->(Player,Player,a)
  mortgage :: a -> (Player,a)
  unmortgage :: a -> (Player,a)


class (Tile a,Deed a) => Buildable a where
  build :: a -> (Player,a)
  destroy :: a -> (Player,a)
  stage :: a-> Int

data ChanceTile = ChanceTile 
  {
    posC :: Int
  } deriving (Show)

instance Tile ChanceTile where
  tileEvent = undefined --pegar da lista de cartas aleatóriamente
  identifier = posC

data NonBuildable = Util 
  {
    donoNB :: Int,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool
  } 
  | Railroad
  {
    donoNB :: Int,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool
  } deriving (Show)

instance Tile NonBuildable where
  tileEvent = undefined --if not owned offer to buy, otherwise if not owner pay according to pattern matching
  identifier = posNB

instance Deed NonBuildable where
  owner = donoNB
  setClass Util {} = UT
  setClass Railroad {} = RR
  trade = undefined --tranfere owners
  mortgage = undefined -- causa mortgage e jogador recebe
  unmortgage = undefined -- paga mortgage e recupera


--to do land
data Land = Land
  { 
    donoB :: Int,
    aluguel :: [Int],
    level :: Int
  } deriving (Show)