module Main (main) where

import PlayerType
import TileType
import Banker
import Jogo
import Graphics.Gloss
import Window
import System.Random
main :: IO ()
--main = do putStrLn $ show $! initialTabuleiro
--main =  display window background board

main = do
  let nPlayers = 2
  print $ id $! initialTabuleiro
  semente1 <- getStdGen
  let dicerng = (randomRs (1,6) semente1 :: [Int])
  semente2 <- getStdGen
  let ccrng = (randomRs (0,15) semente2 :: [Int])
  let tableSetup = setupJogo nPlayers dicerng ccrng
  --reportgame tableSetup
  putStrLn "gamo starto"
  runGame tableSetup 6

runGame :: Jogo -> Int -> IO ()
runGame s 0 = return ()
runGame s countdown = do
  let (d1,d2) = roll2die s
  let p = getNextPlayer s
  let p' = movePlayerBy p (d1+d2)
  let s' = Jogo (tabuleiro s) (updatePlayers p' (jogadores s)) (tail (turnos s)) (drop 2 (rngDice s)) (rngChanceCommunity s)
  reportgame s'
  runGame s' (countdown-1)

reportgame :: Jogo -> IO ()
reportgame s = do
  reportPlayers (jogadores s)

reportPlayers :: [Player] -> IO ()
reportPlayers [] = putStrLn ""
reportPlayers (p:ps) = do
  print p
  reportPlayers ps

