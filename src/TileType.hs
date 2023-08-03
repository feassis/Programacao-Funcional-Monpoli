module TileType (LandTile(..),MiscTile(..),NonBuildable(..),LandTile(..),RealTile(..),PieceSet(..),specialTiles,utilsOrRails,lands,tabuleiro) where

import PlayerType

data PieceSet = Brown | LightBlue | Pink | Orange | Red | Yellow | Green | DarkBlue | UT | RR
  deriving Show

class Tile a where
  tileEvent :: a -> Player -> [Player] -> [Player]
  identifier :: a -> Int --retorna posicao no tabuleiro
  tileName :: a -> String

class Tile a => Deed a where
  owner :: a-> Int
  setClass :: a-> PieceSet
  trade :: a-> Player -> a
  mortgage :: a -> a
  unmortgage :: a -> a


class (Tile a,Deed a) => Buildable a where
  build :: a -> a
  destroy :: a -> a
  stage :: a-> Int

data MiscTile = Chance
  {
    nameM :: String,
    posM :: Int
  }
  | GO 
  {
    nameM :: String,
    posM :: Int
  }
  | Jail 
  {
    nameM :: String,
    posM :: Int
  }
  | FreePark
  {
    nameM :: String,
    posM :: Int
  }
  | Tax
  {
    nameM :: String,
    posM :: Int,
    taxValue :: Int
  }
  | Community
  {
    nameM :: String,
    posM :: Int
  }
  | ToJail
  {
    nameM :: String,
    posM :: Int
  }
  deriving (Show)

instance Tile MiscTile where
  tileEvent = undefined --pegar da lista de cartas aleatóriamente
  identifier = posM
  tileName = nameM

data NonBuildable = Util 
  {
    donoNB :: Int,
    nameNB :: String,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool
  } 
  | Railroad
  {
    donoNB :: Int,
    nameNB :: String,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool
  } deriving (Show)

instance Tile NonBuildable where
  tileEvent = undefined --if not owned offer to buy, otherwise if not owner pay according to pattern matching
  identifier = posNB
  tileName = nameNB

instance Deed NonBuildable where
  owner = donoNB
  setClass Util {} = UT
  setClass Railroad {} = RR
  trade nb p = nb {donoNB = playerID p}
  mortgage nb = nb {isMorgagedNB=True}
  unmortgage nb = nb {isMorgagedNB=False}  

data LandTile = Land
  { 
    donoB :: Int,
    nameB :: String,
    posB :: Int,
    aluguel :: [Int],
    buildcost :: Int,
    level :: Int,
    colorSet :: PieceSet,
    price :: Int,
    morgageValueB :: Int,
    isMorgagedB :: Bool
  } deriving (Show)

instance Tile LandTile where
  identifier = posB
  tileEvent = undefined --if owned pay correct rent up to value to owner, if not offer to buy go to auction if not bought
  tileName = nameB

instance Deed LandTile where
  owner = donoB
  setClass = colorSet
  trade l p = l {donoB = playerID p}
  mortgage l = l {isMorgagedB=True} 
  unmortgage l = l {isMorgagedB=False}

instance Buildable LandTile where
  build (Land d n p a b l c v mv True) = Land d n p a b l c v mv True
  build (Land d n p a b 5 c v mv False) = Land d n p a b 5 c v mv False
  build l = l {level = 1+level l}

  destroy (Land d n p a b 0 c v mv im) = Land d n p a b 5 c v mv im
  destroy l = l {level = (level l)-1}

  stage = level

data RealTile = MTile MiscTile | NBTile NonBuildable | LTile LandTile
instance Tile RealTile where
  tileEvent = tileEvent --pegar da lista de cartas aleatóriamente
  identifier = identifier
  tileName = tileName

instance Show RealTile where
  show (MTile l) = show l
  show (NBTile l) = show l
  show (LTile l) = show l

specialTiles :: [MiscTile]
specialTiles = [
                 GO "Go" 0,
                 Community "Community Chest" 2,
                 Tax "Income Tax" 4 200,
                 Chance "Chance!" 7,
                 Jail "Jail" 10,
                 Community "Community Chest" 17,
                 FreePark "Free Parking" 20,
                 Chance "Chance!" 22,
                 ToJail "Go To Jail" 30,
                 Community "Community Chest" 33,
                 Chance "Chance!" 36,
                 Tax "Luxury Tax" 38 100
               ]

--Bank player is 0
utilsOrRails :: [NonBuildable]
utilsOrRails = [
                Railroad 0 "Reading Railroad" 5 200 100 False,
                Util 0 "Electric Company" 12 150 75 False,
                Railroad 0 "Pennsylvania Railroad" 15 200 100 False,
                Railroad 0 "B. & O. Railroad" 25 200 100 False,
                Util 0 "Water Works" 28 150 75 False,
                Railroad 0 "Short Line" 35 200 100 False
               ]

lands :: [LandTile]
lands = [
         Land 0 "Mediterranean Avenue" 1 [2,4,10,30,90,160,250] 50 0 Brown 60 30 False,
         Land 0 "Baltic Avenue" 3 [4,8,20,60,180,320,450] 50 0 Brown 60 30 False,
         Land 0 "Oriental Avenue" 6 [6,12,30,90,270,400,550] 50 0 LightBlue 100 50 False,
         Land 0 "Vermont Avenue" 8 [6,12,30,90,270,400,550] 50 0 LightBlue 100 50 False,
         Land 0 "Connecticut Avenue" 9 [8,16,40,100,300,450,600] 50 0 LightBlue 120 60 False,
         Land 0 "St Charles Place" 11 [10,20,50,150,450,625,750] 100 0 Pink 140 70 False,
         Land 0 "States Avenue" 13 [10,20,50,150,450,625,750] 100 0 Pink 140 70 False,
         Land 0 "Virginia Avenue" 14 [12,24,60,180,500,700,900] 100 0 Pink 160 80 False,
         Land 0 "St James Place" 16 [14,28,70,200,550,750,950] 100 0 Orange 180 90 False,
         Land 0 "Tennessee Avenue" 18 [14,28,70,200,550,750,950] 100 0 Orange 180 90 False,
         Land 0 "New York Avenue" 19 [16,32,80,220,600,800,1000] 100 0 Orange 200 100 False,
         Land 0 "Kentucky Avenue" 21 [18,36,90,250,700,875,1050] 150 0 Red 220 110 False,
         Land 0 "Indiana Avenue" 23 [18,36,90,250,700,875,1050] 150 0 Red 220 110 False,
         Land 0 "Illinois Avenue" 24 [20,40,100,300,750,925,1100] 150 0 Red 240 120 False,
         Land 0 "Atlantic Avenue" 26 [22,44,110,330,800,975,1150] 150 0 Yellow 260 130 False,
         Land 0 "Ventnor Avenue" 27 [22,44,110,330,800,975,1150] 150 0 Yellow 260 130 False,
         Land 0 "Marvin Gardens" 29 [24,48,120,360,850,1025,1200] 150 0 Yellow 280 140 False,
         Land 0 "Pacific Avenue" 31 [26,52,130,390,900,1100,1275] 200 0 Green 300 150 False,
         Land 0 "North Carolina Avenue" 32 [26,52,130,390,900,1100,1275] 200 0 Green 300 150 False,
         Land 0 "Pennsylvania Avenue" 34 [28,56,150,450,1000,1200,1400] 200 0 Green 320 160 False,
         Land 0 "Park Place" 37 [35,70,175,500,1100,1300,1500] 200 0 DarkBlue 350 175 False,
         Land 0 "Boardwalk" 39 [50,100,200,600,1400,1700,2000] 200 0 DarkBlue 400 200 False
        ]

embaralha :: [a] -> [a]
embaralha xs = p1 ++ p2
  where
    separaemdois [] l1 l2 = (l1,l2)
    separaemdois (y:ys) l1 l2 = separaemdois ys l2 (y:l1)
    (p1,p2) = separaemdois xs [] []



--"quicksort", so que nao aleatorio entao nao muito quick
quicksortTile :: Tile a => [a] -> [a]
quicksortTile [] = []
quicksortTile [a] = [a]
quicksortTile (x:xs) = quicksortTile low ++ [x] ++ quicksortTile high
  where 
    low = [y | y<-xs, identifier y <= identifier x]
    high = [y | y<-xs, identifier y > identifier x]

tabuleiro :: [RealTile]
--tabuleiro = quicksortTile $ (embaralha.embaralha.embaralha) (map MTile specialTiles++map NBTile utilsOrRails++map LTile lands)
tabuleiro = (map MTile specialTiles++map NBTile utilsOrRails++map LTile lands)



