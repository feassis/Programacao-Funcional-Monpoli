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
superviseTrade x Bank p2 y z = superviseTrade x p2 Bank z y
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
handlekeys _ game@(Jogo{gameWon=True}) = game
handlekeys _ game@(Jogo{message=(Message "" [""]),processo = Nothing}) = game {message = (freeRoamMessage.head) (turnos game)} --hardcoding off the visual bug with acknowledgeMessage
handlekeys (EventKey (Char 'c') Down _ _) game@(Jogo {processo = Nothing}) = game {message = (freeRoamMessage.head) (turnos game)} -- manual close to unimportant message
handlekeys (EventKey (SpecialKey KeyUp) Down _ _ ) game@(Jogo {processo = Nothing}) = game {cursor=(cursor game+1) `mod` (length.tabuleiro) game}
handlekeys (EventKey (SpecialKey KeyDown) Down _ _ ) game@(Jogo {processo = Nothing}) = game {cursor=(cursor game-1) `mod` (length.tabuleiro) game}
handlekeys (EventKey (Char 'r') Down _ _) game@(Jogo {processo = Nothing}) = diceRollAction game-- roll dice and have turn
handlekeys (EventKey (Char 'u') Down _ _) game@(Jogo {processo = Nothing}) = upgradeCursorTile game -- attempt to upgrade cursor's tile
handlekeys (EventKey (Char 'd') Down _ _) game@(Jogo {processo = Nothing}) = downgradeCursorTile game
handlekeys (EventKey (Char 'm') Down _ _) game@(Jogo {processo = Nothing}) = mortgageCursorTile game
handlekeys (EventKey (Char 'q') Down _ _) game@(Jogo {processo = Nothing}) = bankruptPlayer game (getNextPlayer game) 0
handlekeys (EventKey (Char 'f') Down _ _) game@(Jogo {processo = Nothing}) = debugForce game (chanceChoice !! 10)
handlekeys _ game@(Jogo {processo = Nothing}) = game --wrongful input does nothing
handlekeys event (Jogo {processo = Just f}) = f event

yesNoQuestion :: Event -> Maybe Bool
yesNoQuestion (EventKey (Char 'y') Down _ _) = Just True
yesNoQuestion (EventKey (Char 'n') Down _ _) = Just False
yesNoQuestion _ = Nothing

acknowledgeMessage :: Jogo -> Event -> Jogo
acknowledgeMessage game (EventKey (Char 'c') Down _ _) = game{message = blankMessage}
acknowledgeMessage game _ = game{processo = Just $ acknowledgeMessage game} --redo, retry

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
      | (isJailed player) && (jailedTurns player>=3) = diceRollMove bf{jogadores=updatePlayers (freeFromJail player) (jogadores bf)}
      | (isJailed player) = jailEscapeProposal bf
      | otherwise = diceRollMove bf

jailEscapeProposal :: Jogo -> Jogo
jailEscapeProposal bf = af
  where
    ms = jailProposalMessage (head.turnos $ bf)
    af = bf {message=ms,processo= Just $ jailProposalQuery bf.yesNoQuestion}

jailProposalQuery :: Jogo -> Maybe Bool -> Jogo
jailProposalQuery bf Nothing = jailEscapeProposal bf --redo, retry
jailProposalQuery bf (Just True) = tryPayJailFine bf --pay to escape
jailProposalQuery bf (Just False) = tryJailBreak bf --try rolling for doubles

diceRollMove :: Jogo -> Jogo
diceRollMove bf = af
  where
    (r1,r2) = roll2die bf
    af = moveInGamePlayerBy (consume2diceroll bf) r1 r2

moveInGamePlayerBy :: Jogo -> Int -> Int -> Jogo
moveInGamePlayerBy bf d1 d2 = af
  where
    roll = d1+d2
    player = (getNextPlayer bf)
    nplayer
      | d1==d2 = movePlayerBy (onFireGuilt player) roll
      | otherwise = movePlayerBy (onFirePardon player) roll
    nturns
      | d1==d2 = (head (turnos bf)):(turnos bf)
      | otherwise = turnos bf
    players = updatePlayers nplayer (jogadores bf)
    af
      | chainedDoubles nplayer >=3 = sendToJail bf True
      | boardPos player > boardPos nplayer = tileEvent.passGoEvent $ bf {jogadores = players,turnos=nturns, dice1 = head $ show d1, dice2 = head $ show d2}
      | otherwise = tileEvent $ bf {jogadores = players,turnos=nturns, dice1 = head $ show d1, dice2 = head $ show d2}

tileEvent :: Jogo -> Jogo
tileEvent bf = af
  where
    rt = getTriggeredTile bf
    af = triggerTile bf rt

triggerTile :: Jogo -> RealTile -> Jogo
triggerTile bf (MTile m) = activateMisc bf m
triggerTile bf t@(NBTile nb)
  | owner nb == 0 && carteira player >= price nb = offerTile bf t --offer to buy
  | playerID player /= owner nb = utilrailRentCharge bf player nb--charge player if unmortgaged
  | otherwise = endTurn $ bf --just end turn
  where
    player = getNextPlayer bf
triggerTile bf t@(LTile l)
  | owner l == 0 = offerTile bf t --offer to buy
  | playerID player /= owner l = rentCharge bf player (owner l) l --charge player if unmortgaged
  | otherwise = endTurn $ bf --just end turn
  where
    player = getNextPlayer bf

utilrailRentCharge :: Jogo -> Player -> NonBuildable -> Jogo
utilrailRentCharge bf jogador nb@(NonBuildable {kindNB=RailRoad}) = af
  where
    ownerId = owner nb
    value = calculaRailroadRent (fetchPlayer ownerId (jogadores bf)) bf
    ownerp = fetchPlayer ownerId (jogadores bf)
    af = attemptChargePlayer bf jogador ownerp value

utilrailRentCharge bf jogador nb@(NonBuildable {kindNB=Util}) = af {rngDice = tail $ rngDice bf}
  where
    ownerId = owner nb
    value = calculaUtilRent (fetchPlayer ownerId (jogadores bf)) bf
    ownerp = fetchPlayer ownerId (jogadores bf)
    af = attemptChargePlayer bf{rngDice = tail (rngDice bf)} jogador ownerp value

utilrailRentCharge bf _ (NonBuildable {kindNB=Build}) = bf --this guy should not exist

rentCharge :: Jogo -> Player -> Int -> Land -> Jogo
rentCharge bf player ownerId tile = af
  where
    value = (aluguel tile) !! (stage tile)
    ownerp = fetchPlayer ownerId (jogadores bf)
    af = attemptChargePlayer bf player ownerp value

calculaRailroadRent :: Player -> Jogo -> Int -- takes owner of tile and the game to return the rent
calculaRailroadRent player jogo = if railroadsOwned == 0 then 0 
  else 25 * (2 ^ (railroadsOwned-1))
    where
      filterRail (NBTile l) = (kindNB l == RailRoad) && (donoNB l == playerID player)
      filterRail _ = False
      railroadsOwned = length $ filter filterRail (tabuleiro jogo)

calculaUtilRent :: Player -> Jogo -> Int -- takes owner of tile and the game to return the rent
calculaUtilRent player jogo = dice *  multiplyer
  where
    filterUtil (NBTile l) = (kindNB l == Util) && (donoNB l == playerID player)
    filterUtil _ = False
    utilitiesOwned = length $ filter filterUtil (tabuleiro jogo)
    multiplyer
      | utilitiesOwned == 0 = 0
      | utilitiesOwned == 1 = 4
      | otherwise = 10
    dice = head (rngDice jogo)

attemptChargePlayer :: Jogo -> Player -> Player -> Int -> Jogo
attemptChargePlayer bf debtguy ownerp value
  | carteira debtguy < value = bff{processo=Just $ solveDebt bff debtguy ownerp value}
  | otherwise = endTurn $ payIngamePlayer bf{jogadores=newPlayers} ownerp value
  where
    newPlayers = updatePlayers newPlayer (jogadores bf)
    newPlayer = chargePlayer debtguy value 
    bff = bf{message=solveDebtMessage (playerID debtguy) value}

solveDebt :: Jogo -> Player -> Player -> Int -> Event -> Jogo
solveDebt bf debtguy ownerp val (EventKey (Char 'c') Down _ _ ) = bf'{processo=Just $ solveDebt bf' debtguy ownerp val}
  where
    bf' = bf{message=solveDebtMessage (playerID debtguy) val}
solveDebt bf debtguy ownerp _ (EventKey (Char 'q') Down _ _) = bankruptPlayer bf debtguy (playerID ownerp)
solveDebt bf debtguy ownerp val (EventKey (SpecialKey KeyUp) Down _ _ ) = bf' {processo=Just $ solveDebt bf' debtguy ownerp val}
  where
    bf' = bf {cursor=(cursor bf+1) `mod` (length.tabuleiro) bf}
solveDebt bf debtguy ownerp val (EventKey (SpecialKey KeyDown) Down _ _ ) = bf'{processo=Just $ solveDebt bf' debtguy ownerp val}
  where
    bf' = bf {cursor=(cursor bf-1) `mod` (length.tabuleiro) bf}
solveDebt bf debtguy ownerp val (EventKey (Char 'd') Down _ _ ) = af
  where
    bf' = downgradeCursorTile bf
    debtguy' = fetchPlayer (playerID debtguy) (jogadores bf')
    newPlayers = updatePlayers newPlayer (jogadores bf')
    newPlayer = chargePlayer debtguy' val 
    af
      | carteira debtguy' < val = bf'{processo=Just $ solveDebt bf' debtguy ownerp val}
      | otherwise = endTurn $ payIngamePlayer bf'{jogadores=newPlayers} ownerp val 
solveDebt bf debtguy ownerp val (EventKey (Char 'm') Down _ _ ) = af
  where
    bf' = mortgageCursorTile bf
    debtguy' = fetchPlayer (playerID debtguy) (jogadores bf')
    newPlayers = updatePlayers newPlayer (jogadores bf')
    newPlayer = chargePlayer debtguy' val 
    af
      | carteira debtguy' < val = bf'{processo=Just $ solveDebt bf' debtguy ownerp val}
      | otherwise = endTurn $ payIngamePlayer bf'{jogadores=newPlayers} ownerp val 
solveDebt bf debtguy ownerp val _ = bf{processo=Just $ solveDebt bf debtguy ownerp val} --redo retry

bankruptPlayer :: Jogo -> Player -> Int -> Jogo
bankruptPlayer bf loser creditorId = af --people can only bankrupt on their turn
  where
    nowner = fetchPlayer creditorId (jogadores bf)
    bla = case loser of
            Bank -> Bank -- bank shouldnt be bankrupted
            _ -> loser {isBankrupted=True}
    (loser',nowner',ntiles) = superviseTrade (tabuleiro bf) bla nowner (deedsAssets loser) []
    nboard = serializedUpdateBoard (tabuleiro bf) ntiles
    remPlayers = takeWhile ((head $ turnos bf)/=) (tail (turnos bf))
    nturns = (head $ turnos bf) : cycle remPlayers
    nplayers = serializedUpdateplayers [loser',nowner'] (jogadores bf)
    af
      | length remPlayers == 1 = bf{tabuleiro=nboard,jogadores=nplayers,turnos=nturns, message=youWinMessage (head remPlayers),gameWon=True,winner=head remPlayers}
      | otherwise = endTurn bf{tabuleiro=nboard,jogadores=nplayers,turnos=nturns}

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

showFunnyMessage :: Jogo -> Message -> Jogo
showFunnyMessage bf ms = bf'{processo = Just $ acknowledgeMessage bf'}
  where
    bf' = bf {message=ms}

sendToJail :: Jogo -> Bool -> Jogo
sendToJail bf onfire = af
  where
    player = getNextPlayer bf
    nplayer = onFirePardon.enJail $ move player jailPos
    nps = updatePlayers nplayer (jogadores bf)
    nturns = (head (turnos bf)):(dropWhile ((head (turnos bf))==) (turnos bf)) --clear onfire extra turns
    arrestMessage = goToJailMessage (playerID player) onfire (head.rngChanceCommunity $ bf)
    af = combineProcess endTurn (`showFunnyMessage` arrestMessage) (consumeCCrng bf{jogadores=nps,turnos=nturns})

payIngamePlayer :: Jogo -> Player -> Int -> Jogo
payIngamePlayer bf p v = bf{jogadores=nps}
  where
    nps = updatePlayers (payPlayer p v) (jogadores bf)

passGoEvent :: Jogo -> Jogo
passGoEvent bf = payIngamePlayer bf (getNextPlayer bf) 200

setIngamePlayerPos :: Jogo -> Player -> Int -> Jogo
setIngamePlayerPos bf p pos = bf{jogadores =updatePlayers (move p pos) (jogadores bf)}

movePlayerCard :: Jogo -> Int -> Jogo
movePlayerCard bf pos = af
  where
    player = getNextPlayer bf
    af
      | boardPos player > pos = tileEvent $ passGoEvent (setIngamePlayerPos bf player pos)
      | otherwise = tileEvent $ (setIngamePlayerPos bf player pos)

combineProcess :: (Jogo -> Jogo) -> (Jogo -> Jogo) -> (Jogo->Jogo)
combineProcess f1 f2 s1 = let s2 = f1 s1
                              s3 = case processo s2 of
                                Nothing -> f2 s2
                                Just f -> s2{processo=Just $ f2.f}
                              in s3

giveGOoJFC :: Jogo -> Jogo
giveGOoJFC bf = bf {jogadores=nps}
  where
    player = getNextPlayer bf
    nplayer = case player of
      Bank -> Bank
      _ -> player {outOfJailCards = outOfJailCards player+1}
    nps = updatePlayers nplayer (jogadores bf)

chanceChoice :: [Jogo -> Jogo]
chanceChoice = [
                (`sendToJail` False)
                , \x -> combineProcess (`movePlayerCard` 39) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to Boardwalk")) x
                , \x -> combineProcess (`movePlayerCard` 0) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to Go (Collect $200)")) x
                , \x -> combineProcess (`movePlayerCard` 24) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to Illinois Avenue. If you pass Go, collect $200")) x
                , \x -> combineProcess (`movePlayerCard` 11) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to St. Charles Place. If you pass Go, collect $200")) x
                , \x -> combineProcess (`movePlayerCard` (nearestRR (boardPos $ getNextPlayer x))) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to the nearest Railroad. If you pass Go, collect $200")) x
                , \x -> combineProcess (`movePlayerCard` (nearestUtil (boardPos $ getNextPlayer x))) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to the nearest Railroad. If you pass Go, collect $200")) x
                , \x -> combineProcess (\y -> attemptChargePlayer y (getNextPlayer x) Bank 15) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Pay poor tax $15")) x
                , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Bank pays you dividend of $50")) $ (payIngamePlayer x (getNextPlayer x) 50)
                , \x -> combineProcess (endTurn.giveGOoJFC) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "You get a Get out of Jail Card")) $ x
                , \x -> combineProcess goBackThree (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Go back 3 spaces")) x
                , \x -> combineProcess (\y -> attemptChargePlayer y (getNextPlayer x) Bank 15) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Speeding fine $15")) x
                , \x -> combineProcess (\y -> attemptChargePlayer y (getNextPlayer x) Bank 30) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Pay to support the dev team $30")) x
                , \x -> combineProcess (`movePlayerCard` 5) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Take a trip to Reading Railroad. If you pass Go, collect $200")) x
                , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "This event is to thank you for playing our game")) x
                , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Your building loan matures. Collect $150")) $ (payIngamePlayer x (getNextPlayer x) 150)
              ]

goBackThree :: Jogo -> Jogo
goBackThree bf = af
  where
    player = getNextPlayer bf
    af = tileEvent $ (setIngamePlayerPos bf player ((boardPos player-3) `mod` (length.tabuleiro) bf)) --not pass go


communityChoice :: [Jogo -> Jogo]
communityChoice = [
                  (`sendToJail` False)
                  , \x -> combineProcess (`movePlayerCard` 0) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Advance to Go (Collect $200)")) x
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Bank error in your favor. Collect $200")) $ (payIngamePlayer x (getNextPlayer x) 200)
                  , \x -> combineProcess (\y -> attemptChargePlayer y (getNextPlayer x) Bank 50) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Doctor's fee. Pay $50")) x
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "From sale of stock you get $50")) $ (payIngamePlayer x (getNextPlayer x) 50)
                  , \x -> combineProcess (endTurn.giveGOoJFC) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "You get a Get out of Jail Card")) $ x
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Holiday fund matures. Receive $100")) $ (payIngamePlayer x (getNextPlayer x) 100)
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Income tax refund. Collect $20")) $ (payIngamePlayer x (getNextPlayer x) 20)
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "This event is to thank you for playing our game")) x
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Life insurance matures. Collect $100")) $ (payIngamePlayer x (getNextPlayer x) 100)
                  , \x -> combineProcess (\y -> attemptChargePlayer y (getNextPlayer x) Bank 100) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Pay hospital fees of $100")) x
                  , \x -> combineProcess (\y -> attemptChargePlayer y (getNextPlayer x) Bank 50) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Pay school fees of $50")) x
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "Receive $25 consultancy fee")) $ (payIngamePlayer x (getNextPlayer x) 25)
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "This event will be implemented in a future dlc costing $50")) x
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "You have won second prize in a beauty contest. Collect $10")) $ (payIngamePlayer x (getNextPlayer x) 10)
                  , \x -> combineProcess endTurn (`showFunnyMessage` (funnyMessageMaker (head.turnos $ x) "You inherit $100")) $ (payIngamePlayer x (getNextPlayer x) 100)
                ]

nearestRR :: Int -> Int
nearestRR ppos
  | ppos<=5 = 5
  | ppos<=15 = 15
  | ppos<=25 = 25
  | ppos<=35 = 35
  | otherwise = 5

nearestUtil:: Int -> Int
nearestUtil ppos
  | ppos <= 12 = 12
  | ppos <= 28 = 28
  | otherwise = 12

activateMisc :: Jogo -> MiscTile -> Jogo
activateMisc bf m = case kindM m of
                      GO -> endTurn.passGoEvent $ bf
                      Chance -> (chanceChoice !! (head $ rngChanceCommunity bf)) (consumeCCrng bf)
                      Community -> (communityChoice !! (head $ rngChanceCommunity bf)) (consumeCCrng bf)
                      FreePark -> endTurn bf
                      ToJail -> sendToJail bf False
                      Jail -> endTurn bf
                      _ -> getCorrectTax bf (identifier m)

getCorrectTax :: Jogo -> Int -> Jogo
getCorrectTax bf pos
  | pos == 4 = combineProcess (\y -> attemptChargePlayer y (getNextPlayer bf) Bank 200) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ bf) "Income tax. Pay $200")) bf
  | otherwise = combineProcess (\y -> attemptChargePlayer y (getNextPlayer bf) Bank 100) (`showFunnyMessage` (funnyMessageMaker (head.turnos $ bf) "Luxury tax. Pay $100")) bf

tryPayJailFine :: Jogo -> Jogo
tryPayJailFine bf = af
  where
    player = getNextPlayer bf
    pPlayer = freeFromJail $ chargePlayer player 50
    af
      | carteira player >= 50 = diceRollAction bf{jogadores= updatePlayers pPlayer (jogadores bf)}
      | otherwise = combineProcess tryJailBreak (`showFunnyMessage` (failedOperation (playerID player))) bf

tryJailBreak :: Jogo -> Jogo
tryJailBreak bf = af
  where
    player = getNextPlayer bf
    escapeP = freeFromJail player
    sitP = sitInJail player
    (d1,d2) = roll2die bf
    af
      | d1==d2 = moveInGamePlayerBy (consume2diceroll bf{jogadores = updatePlayers escapeP (jogadores bf)}) d1 d2
      | otherwise = combineProcess endTurn (`showFunnyMessage` (failedToLeaveJail (playerID sitP) (jailedTurns sitP))) (consume2diceroll bf{jogadores = updatePlayers sitP (jogadores bf)})

debugDefault :: Jogo -> Jogo
debugDefault = endTurn

debugForce :: Jogo -> (Jogo->Jogo) -> Jogo
debugForce bf f = f bf

