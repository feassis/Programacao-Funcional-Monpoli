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
handlekeys (EventKey (Char 'r') Down _ _) game@(Jogo {processo = Nothing}) = diceRollAction game-- roll dice and have turn
handlekeys (EventKey (Char 'u') Down _ _) game@(Jogo {processo = Nothing}) = upgradeCursorTile game -- attempt to upgrade cursor's tile
handlekeys (EventKey (Char 'd') Down _ _) game@(Jogo {processo = Nothing}) = downgradeCursorTile game
handlekeys (EventKey (Char 'm') Down _ _) game@(Jogo {processo = Nothing}) = mortgageCursorTile game
handlekeys (EventKey (Char 'q') Down _ _) game@(Jogo {processo = Nothing}) = undefined
--handlekeys _ game@(Jogo {processo = Nothing}) = game {message = (freeRoamMessage.head) (turnos game)} -- kill popup
handlekeys _ game@(Jogo {processo = Nothing}) = game --wrongful input does nothing
handlekeys event game@(Jogo {processo = Just f}) = f event

yesNoQuestion :: Event -> Maybe Bool
yesNoQuestion (EventKey (Char 'y') Down _ _) = Just True
yesNoQuestion (EventKey (Char 'n') Down _ _) = Just False
yesNoQuestion _ = Nothing

acknowledgeMessage :: Jogo -> Event -> Jogo
acknowledgeMessage game _ = game{message = (freeRoamMessage.head) (turnos game)}

endTurn :: Jogo -> Jogo
endTurn bf = bf {turnos=tail.turnos $ bf, cursor = nc, message = ms, processo=Nothing}
  where
    ntl = tail.turnos $ bf
    nc = boardPos $ fetchPlayer (head ntl) (jogadores bf)  
    ms = freeRoamMessage.head $ ntl

consume2diceroll :: Jogo -> Jogo
consume2diceroll bf = bf {rngDice=tail.tail $ rngDice bf}

consumeCCrng :: Jogo -> Jogo
consumeCCrng bf = bf {rngChanceCommunity = tail.rngChanceCommunity $ bf}

consumeJCardAndFree :: Jogo -> Jogo
consumeJCardAndFree bf = bf {jogadores = psf}
  where
    player = getNextPlayer bf
    pf = useJCard.freeFromJail $ player
    psf = updatePlayers pf (jogadores bf)

diceRollAction :: Jogo -> Jogo
diceRollAction bf = af
  where
    player = getNextPlayer bf
    af
      | (isJailed player) && (outOfJailCards player > 0) = diceRollAction.consumeJCardAndFree $ bf
      | (isJailed player) = jailEscapeProposal bf
      | otherwise = diceRollMove bf

jailEscapeProposal :: Jogo -> Jogo
jailEscapeProposal bf = af
  where
    ms = jailProposalMessage (head.turnos $ bf)
    af = bf {message=ms,processo= Just $ jailProposalQuery bf.yesNoQuestion}

jailProposalQuery :: Jogo -> Maybe Bool -> Jogo
jailProposalQuery bf Nothing = jailEscapeProposal bf --redo, retry
jailProposalQuery bf (Just True) = undefined --pay to escape
jailProposalQuery bf (Just False) = undefined --try rolling for doubles

diceRollMove :: Jogo -> Jogo
diceRollMove bf = af -- it should also handle rolling doubles
  where
    (r1,r2) = roll2die bf
    af = moveInGamePlayerBy (consume2diceroll bf) r1 r2

moveInGamePlayerBy :: Jogo -> Int -> Int -> Jogo
moveInGamePlayerBy bf d1 d2 = af
  where
    roll = d1+d2
    player = movePlayerBy (getNextPlayer bf) roll
    players = updatePlayers player (jogadores bf)
    af = tileEvent $ bf {jogadores = players, dice1 = head $ show d1, dice2 = head $ show d2}

tileEvent :: Jogo -> Jogo
tileEvent bf = af
  where
    rt = getTriggeredTile bf
    af = triggerTile bf rt
    --af = endTurn bf --dummy id to test endturn

triggerTile :: Jogo -> RealTile -> Jogo
triggerTile bf (MTile m) = debugDefault bf
triggerTile bf t@(NBTile nb)
  | owner nb == 0 && carteira player >= price nb = offerTile bf t --offer to buy
  | playerID player /= owner nb = debugDefault bf --charge player if unmortgaged
  | otherwise = endTurn bf --just end turn
  where
    player = getNextPlayer bf
triggerTile bf t@(LTile l)
  | owner l == 0 = offerTile bf t --offer to buy
  | playerID player /= owner l = debugDefault bf --charge player if unmortgaged
  | otherwise = endTurn bf --just end turn
  where
    player = getNextPlayer bf

offerTile :: Jogo -> RealTile -> Jogo
offerTile bf rt = af
  where
    ms = makeBuyOffer rt
    bf' = bf {message = ms}
    af = bf' {processo = Just $ buyProposalQuery bf' rt.yesNoQuestion}

buyProposalQuery :: Jogo -> RealTile -> Maybe Bool -> Jogo
buyProposalQuery bf rt Nothing = bf {processo = Just $ buyProposalQuery bf rt.yesNoQuestion} -- retry, redo
buyProposalQuery bf _ (Just False) = endTurn bf
buyProposalQuery bf rt (Just True) = finishPurchase bf rt

finishPurchase :: Jogo -> RealTile -> Jogo
finishPurchase bf rt@(NBTile nb) = af
  where
    player = getNextPlayer bf
    nrt = NBTile $ trade nb player
    nplayer = chargePlayer (addDeeds player [identifier rt]) (price nb)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = endTurn bf{tabuleiro=nboard,jogadores=nps}
finishPurchase bf rt@(LTile l) = af
  where
    player = getNextPlayer bf
    nrt = LTile $ trade l player
    nplayer = chargePlayer (addDeeds player [identifier rt]) (price l)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = endTurn bf{tabuleiro=nboard,jogadores=nps}
finishPurchase bf _ = endTurn bf --shouldn't happen

upgradeCursorTile :: Jogo -> Jogo
upgradeCursorTile bf = af
  where
    rt = tabuleiro bf !! cursor bf
    af = case rt of
      LTile l -> attemptUpgrade bf l
      NBTile nb -> attemptUnMortgage bf (NBTile nb)
      _ -> printFailOperation bf --otherwise

printFailOperation :: Jogo -> Jogo
printFailOperation bf = bf{message = failedOperation (head $ turnos bf)}

attemptUpgrade :: Jogo -> Land -> Jogo
attemptUpgrade bf land = af
  where
    player = getNextPlayer bf
    af
      | playerID player /= owner land = printFailOperation bf
      | stage land == 5 = bf{message= alreadyMaxLevelMessage (playerID player)}
      | isMorgagedB land && carteira player >= morgageValueB land = finishUnMortgage bf (LTile land)
      | carteira player >= buildcost land = finishUpgrade bf land
      | otherwise = printFailOperation bf

finishUpgrade :: Jogo -> Land -> Jogo
finishUpgrade bf land = af
  where
    nrt = LTile $ build land
    player = getNextPlayer bf
    nplayer = chargePlayer player (buildcost land)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = bf{tabuleiro=nboard,jogadores=nps}

attemptUnMortgage :: Jogo -> RealTile -> Jogo
attemptUnMortgage bf rt@(LTile land) = af --this case shouldn't even happen due to upgrade
  where
    player = getNextPlayer bf
    af
      | owner land /= playerID player = printFailOperation bf
      | not (isMorgagedB land) = bf{message= alreadyMaxLevelMessage (playerID player)}
      | carteira player >= morgageValueB land = finishUnMortgage bf rt
      | otherwise = printFailOperation bf
attemptUnMortgage bf rt@(NBTile nb) = af --this case is useful
  where
    player = getNextPlayer bf
    af
      | owner nb /= playerID player = printFailOperation bf
      | not (isMorgagedNB nb) = bf{message= alreadyMaxLevelMessage (playerID player)}
      | carteira player >= morgageValueNB nb = finishUnMortgage bf rt
      | otherwise = printFailOperation bf
attemptUnMortgage bf _ = bf --case MTile, shouldn't even happen right here

finishUnMortgage :: Jogo -> RealTile -> Jogo
finishUnMortgage bf (LTile land) = af
  where
    nrt = LTile $ unmortgage land
    player = getNextPlayer bf
    nplayer = chargePlayer player (morgageValueB land)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = bf{tabuleiro=nboard,jogadores=nps}
finishUnMortgage bf (NBTile nb) = af
  where
    nrt = NBTile $ unmortgage nb
    player = getNextPlayer bf
    nplayer = chargePlayer player (morgageValueNB nb)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = bf{tabuleiro=nboard,jogadores=nps}
finishUnMortgage bf _ = bf -- MTile. shouldn't even happen

downgradeCursorTile :: Jogo -> Jogo
downgradeCursorTile bf = af
  where
    rt = tabuleiro bf !! cursor bf
    af = case rt of
      LTile l -> attemptDowngrade bf l
      NBTile _ -> bf{message = perhapsMorgageMessage (head $ turnos bf)}
      _ -> printFailOperation bf --otherwise

attemptDowngrade :: Jogo -> Land -> Jogo
attemptDowngrade bf land = af
  where
    player = getNextPlayer bf
    af
      | playerID player /= owner land = printFailOperation bf
      | stage land == 0 = bf{message = perhapsMorgageMessage (playerID player)}
      | otherwise = finishDowngrade bf land

finishDowngrade :: Jogo -> Land -> Jogo
finishDowngrade bf land = af
  where
    nrt = LTile $ destroy land
    player = getNextPlayer bf
    nplayer = payPlayer player (buildcost land)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = bf{tabuleiro=nboard,jogadores=nps}

mortgageCursorTile :: Jogo -> Jogo
mortgageCursorTile bf = af
  where
    rt = tabuleiro bf !! cursor bf
    af = case rt of
      MTile _ -> printFailOperation bf --otherwise
      _ -> attemptMortgage bf rt

attemptMortgage :: Jogo -> RealTile -> Jogo
attemptMortgage bf rt@(LTile land) = af
  where
    player = getNextPlayer bf
    af
      | playerID player /= owner land = printFailOperation bf
      | isMorgagedB land = bf{message = alreadyMortgaged (playerID player)}
      | otherwise = finishMortgage bf rt
attemptMortgage bf rt@(NBTile nb) = af
  where
    player = getNextPlayer bf
    af
      | playerID player /= owner nb = printFailOperation bf
      | isMorgagedNB nb = bf{message = alreadyMortgaged (playerID player)}
      | otherwise = finishMortgage bf rt
attemptMortgage bf _ = bf --case MTile, just skip as it shouldn't happen

finishMortgage :: Jogo -> RealTile -> Jogo
finishMortgage bf (LTile land) = af
  where
    nrt = LTile $ mortgage (land{level=0})
    player = getNextPlayer bf
    nplayer = payPlayer player (morgageValueB land + level land * buildcost land)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = bf{tabuleiro=nboard,jogadores=nps}
finishMortgage bf (NBTile nb) = af
  where
    nrt = NBTile $ mortgage nb
    player = getNextPlayer bf
    nplayer = payPlayer player (morgageValueNB nb)
    nps = updatePlayers nplayer (jogadores bf)
    nboard = updateBoard (tabuleiro bf) nrt
    af = bf{tabuleiro=nboard,jogadores=nps}
finishMortgage bf _ = bf --MTile shouldn't even happen

debugDefault :: Jogo -> Jogo
debugDefault = endTurn

