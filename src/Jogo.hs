module Jogo where

import PlayerType
import TileType


data Jogo = Jogo
  {
    tabuleiro :: [RealTile],
    jogadores :: [Player],
    turnos :: [Int], -- byplayer id
    rngDice :: [Int],
    rngChanceCommunity :: [Int]
  }

setupJogo :: Int -> [Int] -> [Int] -> Jogo
setupJogo n rngd rngc = Jogo initialTabuleiro ps t rngd rngc --da pra fazer aplicacao parcial
  where
    ps = genStartGamePlayer n
    t = cycle ([1..n])

roll2die :: Jogo -> (Int,Int)
roll2die s = (d1,d2)
  where
    d12 = take 2 (rngDice s)
    d1 = head d12
    d2 = last d12

getNextPlayer :: Jogo -> Player
getNextPlayer s = fetchPlayer (head $ turnos s) (jogadores s)