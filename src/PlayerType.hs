module PlayerType (Player(..),payPlayer,chargePlayer,enJail,freeFromJail,onFireGuilt,
                   onFirePardon,move,sitInJail,genIdentifiedPlayer,genStartGamePlayer,
                   removeDeeds,addDeeds,updatePlayers) where

import qualified Data.List as DL

data Player = Player
  {
    playerID :: Int,
    boardPos :: Int,
    chainedDoubles :: Int,
    carteira :: Int,
    deedsAssets :: [Int], --int Identifier
    isJailed :: Bool,
    jailedTurns :: Int
  } | Bank deriving (Show)

instance Eq Player where
  Bank == Bank = True
  Bank == _ = False
  _ == Bank = False
  p == q = playerID p == playerID q

payPlayer :: Player -> Int -> Player
payPlayer Bank _ = Bank
payPlayer p v = p {carteira=carteira p+v}

chargePlayer :: Player -> Int -> Either Player Player
chargePlayer Bank _ = Right Bank
chargePlayer p v
  | carteira p >= v = Right p {carteira=carteira p-v}
  | otherwise = Left p

enJail :: Player -> Player
enJail Bank = Bank
enJail p = p {isJailed=True}

freeFromJail :: Player -> Player
freeFromJail Bank = Bank
freeFromJail p = p {isJailed=False,jailedTurns=0}

sitInJail :: Player -> Player
sitInJail Bank = Bank
sitInJail p = p {jailedTurns=jailedTurns p+1}

move :: Player -> Int -> Player
move Bank _ = Bank
move p posFinal = p {boardPos = posFinal}

onFireGuilt :: Player -> Player
onFireGuilt Bank = Bank
onFireGuilt p = p {chainedDoubles=chainedDoubles p+1}

onFirePardon :: Player -> Player
onFirePardon Bank = Bank
onFirePardon p = p {chainedDoubles = 0} --move logic is delegated to Banker

genIdentifiedPlayer :: Int -> Player --gera player nas condições iniciais
genIdentifiedPlayer 0 = Bank
genIdentifiedPlayer i = Player i 0 0 1500 [] False 0

genStartGamePlayer :: Int -> [Player]
genStartGamePlayer n = [genIdentifiedPlayer i | i<-[0..n]]

removeDeeds :: Player -> [Int] -> Player
removeDeeds Bank _ = Bank
removeDeeds p xs = p {deedsAssets=deedsAssets p DL.\\ xs}

addDeeds :: Player -> [Int] -> Player
addDeeds Bank _ = Bank
addDeeds p xs = p {deedsAssets=deedsAssets p `DL.union` xs }

updatePlayers :: Player -> [Player] -> [Player]
updatePlayers q [] = error "updating non-existent player"
updatePlayers q (p:ps)
  | q == p = q:ps
  | otherwise = p:updatePlayers q ps