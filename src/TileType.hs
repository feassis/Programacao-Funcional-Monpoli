module TileType where

import PlayerType
import qualified Data.List as DT

data ColorSet = Brown | LightBlue | Pink | Orange | Red | Yellow | Green | DarkBlue
  deriving Show
data DeedKind = RailRoad | Util | Build
  deriving Show
data MiscTileKind = Chance | GO | Jail | Community | FreePark | Tax | ToJail
  deriving (Show)

class Tile a where
  --tileEvent "Tile" a -> GameState -> IO (GameState) se encontra em Jogo 
  identifier :: a -> Int --retorna posicao no tabuleiro
  tileName :: a -> String

class Tile a => Deed a where
  owner :: a-> Int
  trade :: a-> Player -> a
  mortgage :: a -> a
  unmortgage :: a -> a
  kind :: a -> DeedKind

class (Tile a,Deed a) => Buildable a where
  build :: a -> a
  destroy :: a -> a
  stage :: a-> Int
  colorset :: a -> ColorSet

data MiscTile = MiscTile
  {
    kindM :: MiscTileKind,
    posM :: Int
  } deriving (Show)

instance Tile MiscTile where
  identifier = posM
  tileName = show.kindM

data NonBuildable = NonBuildable 
  {
    donoNB :: Int,
    kindNB :: DeedKind,
    nameNB :: String,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool
  } deriving Show

instance Tile NonBuildable where
  identifier = posNB
  tileName = nameNB

instance Deed NonBuildable where
  owner = donoNB
  kind = kindNB
  trade nb p = nb {donoNB = playerID p}
  mortgage nb = nb {isMorgagedNB=True}
  unmortgage nb = nb {isMorgagedNB=False} 

data Land = Land
  { 
    donoB :: Int,
    kindB :: DeedKind,
    nameB :: String,
    posB :: Int,
    aluguel :: [Int],
    buildcost :: Int,
    level :: Int,
    colorSet :: ColorSet,
    price :: Int,
    morgageValueB :: Int,
    isMorgagedB :: Bool
  } deriving Show

instance Tile Land where
  identifier = posB
  tileName = nameB

instance Deed Land where
  owner = donoB
  kind = kindB
  trade l p = l {donoB = playerID p}
  mortgage l = l {isMorgagedB=True} 
  unmortgage l = l {isMorgagedB=False}

instance Buildable Land where
  build (Land d k n p a b l c v mv True) = Land d k n p a b l c v mv True
  build (Land d k n p a b 5 c v mv False) = Land d k n p a b 5 c v mv False
  build l = l {level = 1+level l}

  destroy (Land d k n p a b 0 c v mv im) = Land d k n p a b 0 c v mv im
  destroy l = l {level = (level l)-1}

  stage = level
  colorset = colorSet
  
data RealTile = MTile MiscTile | NBTile NonBuildable | LTile Land
instance Tile RealTile where
  identifier (MTile l) = identifier l
  identifier (NBTile l) = identifier l
  identifier (LTile l) = identifier l

  tileName (MTile l) = tileName l
  tileName (NBTile l) = tileName l
  tileName (LTile l) = tileName l

instance Show RealTile where
  show (MTile l) = show l
  show (NBTile l) = show l
  show (LTile l) = show l

instance Eq RealTile where
  a == b = identifier a == identifier b

instance Ord RealTile where
  a <= b = identifier a <= identifier b

specialTiles :: [MiscTile]
specialTiles = [
                 MiscTile GO 0 ,
                 MiscTile Community 2 ,
                 MiscTile Tax 4 , --200 tax
                 MiscTile Chance 7 ,
                 MiscTile Jail 10 ,
                 MiscTile Community 17 ,
                 MiscTile FreePark 20 ,
                 MiscTile Chance 22 ,
                 MiscTile ToJail 30 ,
                 MiscTile Community 33 ,
                 MiscTile Chance 36,
                 MiscTile Tax 38 --100 tax
               ]

utilsOrRails :: [NonBuildable]
utilsOrRails = [
                NonBuildable 0 RailRoad "Reading Railroad" 5 200 100 False ,
                NonBuildable 0 Util "Electric Company" 12 150 75 False ,
                NonBuildable 0 RailRoad "Pennsylvania Railroad" 15 200 100 False ,
                NonBuildable 0 RailRoad "B. & O. Railroad" 25 200 100 False ,
                NonBuildable 0 Util "Water Works" 28 150 75 False,
                NonBuildable 0 RailRoad "Short Line" 35 200 100 False
               ]

lands :: [Land]
lands = [
         Land 0 Build "Mediterranean Avenue" 1 [2,4,10,30,90,160,250] 50 0 Brown 60 30 False ,
         Land 0 Build "Baltic Avenue" 3 [4,8,20,60,180,320,450] 50 0 Brown 60 30 False ,
         Land 0 Build "Oriental Avenue" 6 [6,12,30,90,270,400,550] 50 0 LightBlue 100 50 False ,
         Land 0 Build "Vermont Avenue" 8 [6,12,30,90,270,400,550] 50 0 LightBlue 100 50 False ,
         Land 0 Build "Connecticut Avenue" 9 [8,16,40,100,300,450,600] 50 0 LightBlue 120 60 False ,
         Land 0 Build "St Charles Place" 11 [10,20,50,150,450,625,750] 100 0 Pink 140 70 False ,
         Land 0 Build "States Avenue" 13 [10,20,50,150,450,625,750] 100 0 Pink 140 70 False ,
         Land 0 Build "Virginia Avenue" 14 [12,24,60,180,500,700,900] 100 0 Pink 160 80 False ,
         Land 0 Build "St James Place" 16 [14,28,70,200,550,750,950] 100 0 Orange 180 90 False ,
         Land 0 Build "Tennessee Avenue" 18 [14,28,70,200,550,750,950] 100 0 Orange 180 90 False ,
         Land 0 Build "New York Avenue" 19 [16,32,80,220,600,800,1000] 100 0 Orange 200 100 False ,
         Land 0 Build "Kentucky Avenue" 21 [18,36,90,250,700,875,1050] 150 0 Red 220 110 False ,
         Land 0 Build "Indiana Avenue" 23 [18,36,90,250,700,875,1050] 150 0 Red 220 110 False ,
         Land 0 Build "Illinois Avenue" 24 [20,40,100,300,750,925,1100] 150 0 Red 240 120 False ,
         Land 0 Build "Atlantic Avenue" 26 [22,44,110,330,800,975,1150] 150 0 Yellow 260 130 False ,
         Land 0 Build "Ventnor Avenue" 27 [22,44,110,330,800,975,1150] 150 0 Yellow 260 130 False ,
         Land 0 Build "Marvin Gardens" 29 [24,48,120,360,850,1025,1200] 150 0 Yellow 280 140 False ,
         Land 0 Build "Pacific Avenue" 31 [26,52,130,390,900,1100,1275] 200 0 Green 300 150 False ,
         Land 0 Build "North Carolina Avenue" 32 [26,52,130,390,900,1100,1275] 200 0 Green 300 150 False ,
         Land 0 Build "Pennsylvania Avenue" 34 [28,56,150,450,1000,1200,1400] 200 0 Green 320 160 False ,
         Land 0 Build "Park Place" 37 [35,70,175,500,1100,1300,1500] 200 0 DarkBlue 350 175 False ,
         Land 0 Build "Boardwalk" 39 [50,100,200,600,1400,1700,2000] 200 0 DarkBlue 400 200 False 
        ]

initialTabuleiro :: [RealTile]
initialTabuleiro = DT.sort (map MTile specialTiles++map NBTile utilsOrRails++map LTile lands)

updateBoard :: [RealTile] -> [RealTile] -> [RealTile]
updateBoard og chn = updateBoard' ogo chno
  where
    updateBoard' [] [] = []
    updateBoard' [] (_:_) = error "shit happened at update board"
    updateBoard' xs [] = xs
    updateBoard' (x:xs) (y:ys)
      | identifier x == identifier y = y:updateBoard' xs ys
      | otherwise = x:updateBoard' xs (y:ys)
    ogo = DT.sort og
    chno = DT.sort chn

retrieveTiles :: [Int] -> [RealTile] -> [RealTile]
retrieveTiles ids tab = retrieveTiles' idso tabo
  where
    tabo = DT.sort tab
    idso = DT.sort ids
    retrieveTiles' :: [Int] -> [RealTile] -> [RealTile]
    retrieveTiles' [] _ = []
    retrieveTiles' (_:_) [] = error "shit happened at retrieve tiles"
    retrieveTiles' (x:xs) (y:ys)
      | x == identifier y = y:retrieveTiles' xs ys
      | otherwise = retrieveTiles' (x:xs) ys

serializedTrade :: [RealTile] -> Player -> [RealTile]
serializedTrade [] _ = []
serializedTrade ((MTile m):xs) _ = error "non Deed at Trade"
serializedTrade ((NBTile d):xs) Bank = NBTile d{donoNB=0}:serializedTrade xs Bank
serializedTrade ((NBTile d):xs) p = NBTile d{donoNB=playerID p}:serializedTrade xs p
serializedTrade ((LTile d):xs) Bank = LTile d{donoB=0}:serializedTrade xs Bank
serializedTrade ((LTile d):xs) p = LTile d{donoB=playerID p}:serializedTrade xs p
