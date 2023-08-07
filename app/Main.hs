module Main (main) where

import PlayerType
import TileType
import Graphics.Gloss
import Window

main :: IO ()
--main = putStrLn $ show $! tabuleiro
main =  display window background board
