module Main (main) where

import PlayerType
import TileType
import Jogo
import Graphics.Gloss
import Window
import System.Random
import Banker
main :: IO ()
--main = putStrLn $ show $! tabuleiro
--main =  display window background board


main = do
  let nPlayers = 4
  -- print $ id $! initialTabuleiro
  semente1 <- getStdGen
  let dicerng = (randomRs (1,6) semente1 :: [Int])
  semente2 <- getStdGen
  let ccrng = (randomRs (0,15) semente2 :: [Int])
  let tableSetup = setupJogo nPlayers dicerng ccrng
  play
    (InWindow "Monopoly" (width,height) (0,0))
    black
    fps
    tableSetup
    desenhar
    handlekeys
    (const id)
