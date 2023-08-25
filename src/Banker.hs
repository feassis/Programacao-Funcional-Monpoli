module Banker where
--O objetivo de Banker é ser a interface entre Players e Tiles
--Sua existencia visa driblar import cíclico
import TileType
import PlayerType
import Jogo
import Message
import qualified Data.List as DT
import Graphics.Gloss.Interface.IO.Game

boardsize :: Int
boardsize = length initialTabuleiro

movePlayerBy :: Player -> Int -> Player --encontrar uma forma de implementar o gameevent
movePlayerBy Bank _ = Bank
movePlayerBy p roll
  | l+roll<boardsize = p {boardPos=l+roll}
  | otherwise = p {boardPos=(l+roll) `mod` boardsize}
  where
    l = boardPos p


-- will supervise the process only, validation of input is other responsability

superviseTrade :: [RealTile] -> Player -> Player -> [Int] -> [Int] -> (Player,Player,[RealTile])
superviseTrade _ Bank Bank _ _ = (Bank,Bank,[]) -- joke:insider trading return reportToAuthoratiesAndSue
superviseTrade x Bank p2 y z = superviseTrade x p2 Bank y z
superviseTrade tab p1 Bank deedsid1 deedsid2 = (p1', Bank, updatedTiles)
  where
    deeds1 = retrieveTiles deedsid1 tab
    deeds2 = retrieveTiles deedsid2 tab
    ndeeds1to2 = serializedTrade deeds1 Bank
    ndeeds2to1 = serializedTrade deeds2 p1
    updatedTiles = ndeeds1to2 ++ ndeeds2to1
    p1' = p1 {deedsAssets = (deedsAssets p1 DT.\\ deedsid1) `DT.union` deedsid2}
superviseTrade tab p1 p2 deedsid1 deedsid2 = (p1',p2',updatedTiles)
  where
    deeds1 = retrieveTiles deedsid1 tab
    deeds2 = retrieveTiles deedsid2 tab
    ndeeds1to2 = serializedTrade deeds1 p2
    ndeeds2to1 = serializedTrade deeds2 p1
    updatedTiles = ndeeds1to2 ++ ndeeds2to1
    p1' = p1 {deedsAssets = (deedsAssets p1 DT.\\ deedsid1) `DT.union` deedsid2}
    p2' = p2 {deedsAssets = (deedsAssets p2 DT.\\ deedsid2) `DT.union` deedsid1}


handlekeys :: Event -> Jogo -> Jogo
handlekeys (EventKey (Char 'c') Down _ _) game@(Jogo {processo = Nothing}) = game {message = (freeRoamMessage.head) (turnos game)} -- manual close to unimportant message
handlekeys (EventKey (SpecialKey KeyUp) Down _ _ ) game@(Jogo {processo = Nothing}) = game {cursor=(cursor game+1) `mod` (length.tabuleiro) game}
handlekeys (EventKey (SpecialKey KeyDown) Down _ _ ) game@(Jogo {processo = Nothing}) = game {cursor=(cursor game-1) `mod` (length.tabuleiro) game}
handlekeys (EventKey (Char 'r') Down _ _) game@(Jogo {processo = Nothing}) = undefined -- roll dice and have turn
handlekeys (EventKey (Char 'u') Down _ _) game@(Jogo {processo = Nothing}) = undefined -- attempt to upgrade cursor's tile
handlekeys (EventKey (Char 'd') Down _ _) game@(Jogo {processo = Nothing}) = undefined
handlekeys (EventKey (Char 'm') Down _ _) game@(Jogo {processo = Nothing}) = undefined
handlekeys (EventKey (Char 'q') Down _ _) game@(Jogo {processo = Nothing}) = undefined
handlekeys _ game@(Jogo {processo = Nothing}) = game {message = (freeRoamMessage.head) (turnos game)} -- kill popup
handlekeys _ game@(Jogo {processo = Nothing}) = game --wrongful input does nothing
handlekeys event game@(Jogo {processo = Just f}) = f event

yesNoQuestion :: Event -> Maybe Bool
yesNoQuestion (EventKey (Char 'y') Down _ _) = Just True
yesNoQuestion (EventKey (Char 'n') Down _ _) = Just False
yesNoQuestion _ = Nothing

endTurn :: Jogo -> Jogo
endTurn bf = bf {turnos=ntl,message=nm,processo=Nothing}
  where
    ntl = tail.turnos $ bf
    nm = freeRoamMessage.head $ ntl

consume2diceroll :: Jogo -> Jogo
consume2diceroll bf = bf {rngDice=tail.tail $ rngDice bf}

consumeCCrng :: Jogo -> Jogo
consumeCCrng bf = bf {rngChanceCommunity = tail.rngChanceCommunity $ bf}

--diceRollAction :: Jogo -> Jogo
--diceRollAction bf = af
--  where
--    player = getNextPlayer bf
