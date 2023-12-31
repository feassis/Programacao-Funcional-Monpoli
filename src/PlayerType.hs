module PlayerType (Player(..),payPlayer,chargePlayer,enJail,freeFromJail,onFireGuilt,
                   onFirePardon,move,sitInJail,genIdentifiedPlayer,genStartGamePlayer,
                   removeDeeds,addDeeds,updatePlayers,fetchPlayer,useJCard,serializedUpdateplayers) where

import qualified Data.List as DL

data Player = Player
  {
    playerID :: Int,
    boardPos :: Int,
    chainedDoubles :: Int,
    carteira :: Int,
    deedsAssets :: [Int], --int Identifier
    isJailed :: Bool,
    jailedTurns :: Int,
    outOfJailCards :: Int,
    isBankrupted :: Bool
  } | Bank deriving (Show)

instance Eq Player where
  Bank == Bank = True
  Bank == _ = False
  _ == Bank = False
  p == q = playerID p == playerID q

payPlayer :: Player -> Int -> Player
payPlayer Bank _ = Bank
payPlayer p v = p {carteira=carteira p+v}

chargePlayer :: Player -> Int -> Player --I assume you already confirmed it can be charged
chargePlayer Bank _ = Bank
chargePlayer p v = p {carteira=carteira p-v}

enJail :: Player -> Player
enJail Bank = Bank
enJail p = p {isJailed=True}

freeFromJail :: Player -> Player
freeFromJail Bank = Bank
freeFromJail p = p {isJailed=False,jailedTurns=0}

sitInJail :: Player -> Player
sitInJail Bank = Bank
sitInJail p = p {jailedTurns=jailedTurns p+1}

useJCard :: Player -> Player
useJCard Bank = Bank
useJCard p
  | outOfJailCards p <= 0 = error "using non-existent jail card"
  | otherwise = p{outOfJailCards = outOfJailCards p-1}

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
genIdentifiedPlayer i = Player i 0 0 1500 [] False 0 0 False

genStartGamePlayer :: Int -> [Player]
genStartGamePlayer n = [genIdentifiedPlayer i | i<-[0..n]]

removeDeeds :: Player -> [Int] -> Player
removeDeeds Bank _ = Bank
removeDeeds p xs = p {deedsAssets=deedsAssets p DL.\\ xs}

addDeeds :: Player -> [Int] -> Player
addDeeds Bank _ = Bank
addDeeds p xs = p {deedsAssets=deedsAssets p `DL.union` xs }

updatePlayers :: Player -> [Player] -> [Player]
updatePlayers q [] = []
updatePlayers q (p:ps)
  | q == p = q:ps
  | otherwise = p:updatePlayers q ps

serializedUpdateplayers :: [Player] -> [Player] -> [Player]
serializedUpdateplayers qs ps = foldl (flip updatePlayers) ps qs
--serializedUpdateplayers [] ps = ps
--serializedUpdateplayers (q:qs) ps = serializedUpdateplayers qs (updatePlayers q ps)

fetchPlayer :: Int -> [Player] -> Player
fetchPlayer _ [] = Bank --effectively an error
fetchPlayer i (Bank:ps)
  | i == 0 = Bank
  | otherwise = fetchPlayer i ps
fetchPlayer i (p:ps)
  | i== playerID p = p
  | otherwise = fetchPlayer i ps