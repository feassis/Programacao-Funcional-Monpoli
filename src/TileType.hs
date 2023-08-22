module TileType (Tile(..),Deed(..),Buildable(..),LandTile(..),MiscTile(..),NonBuildable(..),
                 RealTile(..),PieceSet(..),specialTiles,utilsOrRails,lands,tabuleiro,updateBoard,
                 retrieveTiles,serializedTrade) where

import PlayerType
import qualified Data.List as DT

data PieceSet = Brown | LightBlue | Pink | Orange | Red | Yellow | Green | DarkBlue | UT | RR
  deriving Show

newtype GameEvent = GameEvent (Player -> [Player] -> [RealTile] -> Either GameEvent ([Player],[RealTile]))

class Tile a where
  tileEvent :: a -> GameEvent
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
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }
  | GO 
  {
    nameM :: String,
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }
  | Jail 
  {
    nameM :: String,
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }
  | FreePark
  {
    nameM :: String,
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }
  | Tax
  {
    nameM :: String,
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }
  | Community
  {
    nameM :: String,
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }
  | ToJail
  {
    nameM :: String,
    posM :: Int,
    eventM :: MiscTile -> GameEvent
  }

instance Tile MiscTile where
  tileEvent x = eventM x x
  identifier = posM
  tileName = nameM

data NonBuildable = Util 
  {
    donoNB :: Int,
    nameNB :: String,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool,
    eventNB :: NonBuildable -> GameEvent
  } 
  | Railroad
  {
    donoNB :: Int,
    nameNB :: String,
    posNB :: Int,
    priceNB :: Int,
    morgageValueNB :: Int,
    isMorgagedNB :: Bool,
    eventNB :: NonBuildable -> GameEvent
  }

instance Tile NonBuildable where
  tileEvent x = eventNB x x--if not owned offer to buy, otherwise if not owner pay according to pattern matching
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
    isMorgagedB :: Bool,
    eventL :: LandTile -> GameEvent
  } 

instance Tile LandTile where
  identifier = posB
  tileEvent x = eventL x x--if owned pay correct rent up to value to owner, if not offer to buy go to auction if not bought
  tileName = nameB

instance Deed LandTile where
  owner = donoB
  setClass = colorSet
  trade l p = l {donoB = playerID p}
  mortgage l = l {isMorgagedB=True} 
  unmortgage l = l {isMorgagedB=False}

instance Buildable LandTile where
  build (Land d n p a b l c v mv True f) = Land d n p a b l c v mv True f
  build (Land d n p a b 5 c v mv False f) = Land d n p a b 5 c v mv False f
  build l = l {level = 1+level l}

  destroy (Land d n p a b 0 c v mv im f) = Land d n p a b 0 c v mv im f
  destroy l = l {level = (level l)-1}

  stage = level

data RealTile = MTile MiscTile | NBTile NonBuildable | LTile LandTile
instance Tile RealTile where
  tileEvent = tileEvent --faça o que era para se fazer
  identifier = identifier
  tileName = tileName

--instance Show RealTile where
--  show (MTile l) = show l
--  show (NBTile l) = show l
--  show (LTile l) = show l

instance Eq RealTile where
  a == b = identifier a == identifier b

instance Ord RealTile where
  a <= b = identifier a == identifier b

specialTiles :: [MiscTile]
specialTiles = [
                 GO "Go" 0 goEvent,
                 Community "Community Chest" 2 communityChestEvent,
                 Tax "Income Tax" 4 (taxEvent 200),
                 Chance "Chance!" 7 chanceEvent,
                 Jail "Jail" 10 jailEvent,
                 Community "Community Chest" 17 communityChestEvent,
                 FreePark "Free Parking" 20 freeParkEvent,
                 Chance "Chance!" 22 chanceEvent,
                 ToJail "Go To Jail" 30 toJailEvent,
                 Community "Community Chest" 33 communityChestEvent,
                 Chance "Chance!" 36 chanceEvent,
                 Tax "Luxury Tax" 38 (taxEvent 100)
               ]

--Bank player is 0 

utilsOrRails :: [NonBuildable]
utilsOrRails = [
                Railroad 0 "Reading Railroad" 5 200 100 False railRoadEvent,
                Util 0 "Electric Company" 12 150 75 False utilEvent,
                Railroad 0 "Pennsylvania Railroad" 15 200 100 False railRoadEvent,
                Railroad 0 "B. & O. Railroad" 25 200 100 False railRoadEvent,
                Util 0 "Water Works" 28 150 75 False utilEvent,
                Railroad 0 "Short Line" 35 200 100 False railRoadEvent
               ]

lands :: [LandTile]
lands = [
         Land 0 "Mediterranean Avenue" 1 [2,4,10,30,90,160,250] 50 0 Brown 60 30 False landEvent,
         Land 0 "Baltic Avenue" 3 [4,8,20,60,180,320,450] 50 0 Brown 60 30 False landEvent,
         Land 0 "Oriental Avenue" 6 [6,12,30,90,270,400,550] 50 0 LightBlue 100 50 False landEvent,
         Land 0 "Vermont Avenue" 8 [6,12,30,90,270,400,550] 50 0 LightBlue 100 50 False landEvent,
         Land 0 "Connecticut Avenue" 9 [8,16,40,100,300,450,600] 50 0 LightBlue 120 60 False landEvent,
         Land 0 "St Charles Place" 11 [10,20,50,150,450,625,750] 100 0 Pink 140 70 False landEvent,
         Land 0 "States Avenue" 13 [10,20,50,150,450,625,750] 100 0 Pink 140 70 False landEvent,
         Land 0 "Virginia Avenue" 14 [12,24,60,180,500,700,900] 100 0 Pink 160 80 False landEvent,
         Land 0 "St James Place" 16 [14,28,70,200,550,750,950] 100 0 Orange 180 90 False landEvent,
         Land 0 "Tennessee Avenue" 18 [14,28,70,200,550,750,950] 100 0 Orange 180 90 False landEvent,
         Land 0 "New York Avenue" 19 [16,32,80,220,600,800,1000] 100 0 Orange 200 100 False landEvent,
         Land 0 "Kentucky Avenue" 21 [18,36,90,250,700,875,1050] 150 0 Red 220 110 False landEvent,
         Land 0 "Indiana Avenue" 23 [18,36,90,250,700,875,1050] 150 0 Red 220 110 False landEvent,
         Land 0 "Illinois Avenue" 24 [20,40,100,300,750,925,1100] 150 0 Red 240 120 False landEvent,
         Land 0 "Atlantic Avenue" 26 [22,44,110,330,800,975,1150] 150 0 Yellow 260 130 False landEvent,
         Land 0 "Ventnor Avenue" 27 [22,44,110,330,800,975,1150] 150 0 Yellow 260 130 False landEvent,
         Land 0 "Marvin Gardens" 29 [24,48,120,360,850,1025,1200] 150 0 Yellow 280 140 False landEvent,
         Land 0 "Pacific Avenue" 31 [26,52,130,390,900,1100,1275] 200 0 Green 300 150 False landEvent,
         Land 0 "North Carolina Avenue" 32 [26,52,130,390,900,1100,1275] 200 0 Green 300 150 False landEvent,
         Land 0 "Pennsylvania Avenue" 34 [28,56,150,450,1000,1200,1400] 200 0 Green 320 160 False landEvent,
         Land 0 "Park Place" 37 [35,70,175,500,1100,1300,1500] 200 0 DarkBlue 350 175 False landEvent,
         Land 0 "Boardwalk" 39 [50,100,200,600,1400,1700,2000] 200 0 DarkBlue 400 200 False landEvent
        ]


tabuleiro :: [RealTile]
tabuleiro = DT.sort (map MTile specialTiles++map NBTile utilsOrRails++map LTile lands)
--tabuleiro = (map MTile specialTiles++map NBTile utilsOrRails++map LTile lands)_

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
-- data RealTile = MTile MiscTile | NBTile NonBuildable | LTile LandTile

goEvent :: MiscTile -> GameEvent
goEvent _ = GameEvent f
  where
    f p ps tab = Right (updatePlayers (payPlayer p 200) ps,tab)

communityChestEvent :: MiscTile -> GameEvent
communityChestEvent = undefined

taxEvent :: Int -> MiscTile -> GameEvent
taxEvent = undefined

chanceEvent :: MiscTile -> GameEvent
chanceEvent = undefined

jailEvent :: MiscTile -> GameEvent
jailEvent = undefined

toJailEvent :: MiscTile -> GameEvent
toJailEvent = undefined

freeParkEvent :: MiscTile -> GameEvent
freeParkEvent = undefined

railRoadEvent :: NonBuildable -> GameEvent
railRoadEvent = undefined

utilEvent :: NonBuildable -> GameEvent
utilEvent = undefined

landEvent :: LandTile -> GameEvent
landEvent = undefined

--Note to future Gigeck
--If scenariocomes unresolved return a left GameEvent also resolve that in OI
--If scenariocomes resolve return right and new gamestate (Players,Board)