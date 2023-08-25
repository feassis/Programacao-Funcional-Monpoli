module Window where

import Graphics.Gloss
import PlayerType
import TileType
import Jogo

type Line = (Float, Float, Float) -- (x, y, size)

-- Largura
width :: Int 
width = 1920

widthF :: Float 
widthF = 1920

-- Altura
height :: Int
height = 1080

heightF :: Float
heightF = 1080

-- Cor de fundo
background :: Color
background = black

-- Criar Janela
window = InWindow "Monopoli" (width, height) (0 , 0)

fps :: Int 
fps = 30

lineColor :: Color
lineColor = white



-- Instância 2: Jogador preso
player2 :: Player
player2 = Player
  {
    playerID = 2,
    boardPos = 1,
    chainedDoubles = 3,
    carteira = 800,
    deedsAssets = [],
    isJailed = True,
    jailedTurns = 2
  }

-- Instância 3: Jogador sem propriedades
player3 :: Player
player3 = Player
  {
    playerID = 3,
    boardPos = 0,
    chainedDoubles = 1,
    carteira = 2000,
    deedsAssets = [],
    isJailed = False,
    jailedTurns = 0
  }

-- Instância 4: Banco
bank :: Player
bank = Bank


horizontalBorder :: Float -> Picture
horizontalBorder offset = translate 0 offset
                $ color lineColor
                $ rectangleSolid boardWidth 2

horizontalLine :: Float -> Float -> Picture
horizontalLine posX posY = translate posX posY
                $ color lineColor
                $ rectangleSolid cellSize 2

horizontalLineColored :: Float -> Float -> Color -> Picture
horizontalLineColored posX posY cor = translate posX posY
                $ color cor
                $ rectangleSolid cellSize 8

verticalBorder :: Float -> Picture
verticalBorder offset = translate offset 0
                        $ color lineColor
                        $ rectangleSolid 2
                        $ boardHeight

verticalLine :: Float -> Float -> Picture
verticalLine  posX posY = translate posX posY
                        $ color lineColor
                        $ rectangleSolid 2
                        $ cellSize

verticalLineColored :: Float -> Float -> Color -> Picture
verticalLineColored  posX posY cor = translate posX posY
                        $ color cor
                        $ rectangleSolid 8
                        $ cellSize

cellSize :: Float
cellSize = 80

boardWidth :: Float
boardWidth = cellSize * 11
boardHeight = cellSize * 11

horizontalLines :: [Picture]
horizontalLines = rightLines ++ leftLines

rightLines :: [Picture]
rightLines = [  horizontalLine (cellSize *(5)) (cellSize/2), horizontalLine (cellSize *(5)) (-cellSize/2),
                horizontalLine (cellSize *(5)) (cellSize/2*3), horizontalLine (cellSize *(5)) (-cellSize/2*3),
                horizontalLine (cellSize *(5)) (cellSize/2*5), horizontalLine (cellSize *(5)) (-cellSize/2*5),
                horizontalLine (cellSize *(5)) (cellSize/2*7), horizontalLine (cellSize *(5)) (-cellSize/2*7),
                horizontalLine (cellSize *(5)) (cellSize/2*9), horizontalLine (cellSize *(5)) (-cellSize/2*9),
                horizontalLine (cellSize *(5)) (cellSize/2*11), horizontalLine (cellSize *(5)) (-cellSize/2*11)
            ]

leftLines :: [Picture]
leftLines = [  horizontalLine (-cellSize *(5)) (cellSize/2), horizontalLine (-cellSize *(5)) (-cellSize/2),
                horizontalLine (-cellSize *(5)) (cellSize/2*3), horizontalLine (-cellSize *(5)) (-cellSize/2*3),
                horizontalLine (-cellSize *(5)) (cellSize/2*5), horizontalLine (-cellSize *(5)) (-cellSize/2*5),
                horizontalLine (-cellSize *(5)) (cellSize/2*7), horizontalLine (-cellSize *(5)) (-cellSize/2*7),
                horizontalLine (-cellSize *(5)) (cellSize/2*9), horizontalLine (-cellSize *(5)) (-cellSize/2*9),
                horizontalLine (-cellSize *(5)) (cellSize/2*11), horizontalLine (-cellSize *(5)) (-cellSize/2*11)
            ]

verticalLines :: [Picture]
verticalLines = upLines ++ downLines

upLines :: [Picture]
upLines = [(verticalLine (cellSize/2) (cellSize *(5))), (verticalLine (-cellSize/2) (cellSize *(5))),
            (verticalLine (cellSize/2*3) (cellSize *(5))), (verticalLine (-cellSize/2*3) (cellSize *(5))),
            (verticalLine (cellSize/2*5) (cellSize *(5))), (verticalLine (-cellSize/2*5) (cellSize *(5))),
            (verticalLine (cellSize/2*7) (cellSize *(5))), (verticalLine (-cellSize/2*7) (cellSize *(5))),
            (verticalLine (cellSize/2*9) (cellSize *(5))), (verticalLine (-cellSize/2*9) (cellSize *(5))),
            (verticalLine (cellSize/2*11) (cellSize *(5))), (verticalLine (-cellSize/2*11) (cellSize *(5)))
        ]

downLines :: [Picture]
downLines = [(verticalLine (cellSize/2) (-cellSize *(5))), (verticalLine (-cellSize/2) (-cellSize *(5))),
            (verticalLine (cellSize/2*3) (-cellSize *(5))), (verticalLine (-cellSize/2*3) (-cellSize *(5))),
            (verticalLine (cellSize/2*5) (-cellSize *(5))), (verticalLine (-cellSize/2*5) (-cellSize *(5))),
            (verticalLine (cellSize/2*7) (-cellSize *(5))), (verticalLine (-cellSize/2*7) (-cellSize *(5))),
            (verticalLine (cellSize/2*9) (-cellSize *(5))), (verticalLine (-cellSize/2*9) (-cellSize *(5))),
            (verticalLine (cellSize/2*11) (-cellSize *(5))), (verticalLine (-cellSize/2*11) (-cellSize *(5)))
        ]

rentHousesLines :: [Picture]
rentHousesLines = blueHouses ++ greenHouses ++ browHouses ++ lightBlueHouses ++ redHouses ++ yellowHouses ++ pinkHouses ++ orangeHouses

renderTxt posX posY c t = translate posX posY
                $ scale 0.3 0.3
                $ color c
                $ text t

renderTxtSmall posX posY c t = translate posX posY
                $ scale 0.1 0.1
                $ color c
                $ text t
renderCircleSmall posX posY c radious = translate posX posY
                $ scale 0.3 0.3
                $ color c
                $ circle radious
renderSquareCell :: (Float, Float) -> Color -> Float -> Picture
renderSquareCell (posX, posY) c size = translate posX posY
                $ scale 1 1
                $ color c
                $ rectangleWire size size
tileText :: [Picture]
tileText = currentTileText ++ zoomTileText

currentTileText :: [Picture] -- mudar para receber Real
currentTileText = [renderTxt (widthF * 0.25) (heightF * 0.35) blue "Current Tile"] ++
                drawLandTileText (widthF * 0.25) (heightF * 0.33) testLandTile

testMiscTile :: MiscTile
testMiscTile = MiscTile { kindM = Chance,  posM = 8}

testNonBuildable :: NonBuildable
testNonBuildable = NonBuildable  {donoNB = -1, 
        kindNB = Util,
        nameNB = "Util", 
        posNB = 1, 
        priceNB = 100, 
        morgageValueNB = 80,
        isMorgagedNB = False}

zoomTileText :: [Picture]
zoomTileText = [renderTxt (widthF * 0.25) (heightF * 0.10) blue "Zoom Tile"] ++
                drawNonBuildableText (widthF * 0.25) (heightF * 0.085) testNonBuildable

drawMiscTile :: Float -> Float -> MiscTile -> [Picture]
drawMiscTile posX posY miscTile = [
                renderTxtSmall posX posY white ("Name: " ++ show(kindM miscTile)), 
                renderTxtSmall posX (posY - (heightF * 0.015)) white ("Position: " ++ show(posM miscTile))
        ]

testLandTile :: Land
testLandTile = Land {
                donoB = -1,
                nameB = "Orlando",
                posB = 24,
                aluguel = [10, 20, 30, 40, 50],
                buildcost = 20,
                level = 2,
                colorSet = DarkBlue,
                price = 100,
                morgageValueB = 50,
                isMorgagedB = True
        }

drawLandTileText :: Float -> Float -> Land -> [Picture]
drawLandTileText posX posY landTile = [
                renderTxtSmall posX posY white ("Name: " ++ (nameB landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015)) white ("Owner: " ++ show(donoB landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 2)) white ("Position: " ++ show(posB landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 3)) white ("Rent: " ++ show(aluguel landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 4)) white ("Build Cost: " ++ show(buildcost landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 5)) white ("Level: " ++ show(level landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 6)) white ("Color: " ++ show(colorSet landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 7)) white ("Price: " ++ show(price landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 8)) white ("Morgage: " ++ show(morgageValueB landTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 9)) white ("Is Morgaged: " ++ show(isMorgagedB landTile))                
        ] 

drawNonBuildableText :: Float -> Float -> NonBuildable -> [Picture]
drawNonBuildableText posX posY nbTile = [
                renderTxtSmall posX posY white ("Name: " ++ (nameNB nbTile)),
                renderTxtSmall posX (posY - (heightF * 0.015)) white ("Owner: " ++ show(donoNB nbTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 2)) white ("Position: " ++ show(posNB nbTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 3)) white ("Price: " ++ show(priceNB nbTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 4)) white ("Morgage: " ++ show(morgageValueNB nbTile)),
                renderTxtSmall posX (posY - (heightF * 0.015 * 5)) white ("Is Morgaged: " ++ show(isMorgagedNB nbTile))
        ]
-- Instância 1: Jogador normal
player1 :: Player
player1 = Player
  {
    playerID = 1,
    boardPos = 39,
    chainedDoubles = 0,
    carteira = 1500,
    deedsAssets = [1, 2],
    isJailed = False,
    jailedTurns = 0
  }

drawPlayerToken :: (Float, Float) -> Int -> Color -> [Picture]
drawPlayerToken (posX, posY) playerId color = [
                        renderCircleSmall (posX + cellSize * 0.2 * fromIntegral(playerId)) posY color 20 
                ]    

selectionVisuals :: [Picture]
selectionVisuals = drawSelectionVisuals 39


drawSelectionVisuals :: Int -> [Picture]
drawSelectionVisuals pos  =  [renderSquareCell (getSelectionPosition !! pos) yellow (cellSize)]

playersVisuals :: [Picture]
playersVisuals = playersText ++ (playerToken player1 blue) ++ (playerToken player2 green) ++ (playerToken player3 yellow) 

getSelectionPosition :: [(Float, Float)]
getSelectionPosition = [
                ((cellSize *(5)), (-cellSize*5)), --0
                ((cellSize *(4)), (-cellSize*5)), --1
                ((cellSize *(3)), (-cellSize*5)), --2
                ((cellSize *(2)), (-cellSize*5)), --3
                ((cellSize *(1)), (-cellSize*5)), --4
                ((cellSize *(0)), (-cellSize*5)), --5
                ((cellSize *(-1)), (-cellSize*5)), --6
                ((cellSize *(-2)), (-cellSize*5)), --7
                ((cellSize *(-3)), (-cellSize*5)), --8
                ((cellSize *(-4)), (-cellSize*5)), --9
                ((cellSize *(-5)), (-cellSize*5)), --10
                ((cellSize *(-5)), (-cellSize*4)), --11
                ((cellSize *(-5)), (-cellSize*3)), --12
                ((cellSize *(-5)), (-cellSize*2)), --13
                ((cellSize *(-5)), (-cellSize*1)), --14
                ((cellSize *(-5)), (-cellSize*0)), --15
                ((cellSize *(-5)), (cellSize*1)), --16
                ((cellSize *(-5)), (cellSize*2)), --17
                ((cellSize *(-5)), (cellSize*3)), --18
                ((cellSize *(-5)), (cellSize*4)), --19
                ((cellSize *(-5)), (cellSize*5)), -- 20
                ((cellSize *(-4)), (cellSize*5)), --21
                ((cellSize *(-3)), (cellSize*5)), --22
                ((cellSize *(-2)), (cellSize*5)), --23
                ((cellSize *(-1)), (cellSize*5)), --24
                ((cellSize *(0)), (cellSize*5)), --25
                ((cellSize *(1)), (cellSize*5)), --26
                ((cellSize *(2)), (cellSize*5)), --27 
                ((cellSize *(3)), (cellSize*5)), --28
                ((cellSize *(4)), (cellSize*5)), --29
                ((cellSize *(5)), (cellSize*5)), --30
                ((cellSize *(5)), (cellSize*4)),--31
                ((cellSize *(5)), (cellSize*3)),--32
                ((cellSize *(5)), (cellSize*2)),--33
                ((cellSize *(5)), (cellSize*1)),--34
                ((cellSize *(5)), (cellSize*0)),--35
                ((cellSize *(5)), (cellSize*(-1))),--36
                ((cellSize *(5)), (cellSize*(-2))),--37
                ((cellSize *(5)), (cellSize*(-3))),--38
                ((cellSize *(5)), (cellSize*(-4)))--39
        ]

getPlayerPosition :: [(Float, Float)]
getPlayerPosition = [
                ((cellSize *(4.5)), (-cellSize/2*10.75)), -- Pos 0
                ((cellSize *(3.5)), (-cellSize/2*10.75)), -- Pos 1
                ((cellSize *(2.5)), (-cellSize/2*10.75)), -- Pos 2
                ((cellSize *(1.5)), (-cellSize/2*10.75)), -- Pos 3
                ((cellSize *(0.5)), (-cellSize/2*10.75)), -- Pos 4
                ((cellSize *(-0.5)), (-cellSize/2*10.75)),-- Pos 5
                ((cellSize *(-1.5)), (-cellSize/2*10.75)),-- Pos 6
                ((cellSize *(-2.5)), (-cellSize/2*10.75)),-- Pos 7
                ((cellSize *(-3.5)), (-cellSize/2*10.75)),-- Pos 8
                ((cellSize *(-4.5)), (-cellSize/2*10.75)),-- Pos 9
                ((cellSize *(-5.5)), (-cellSize/2*10.75)),-- Pos 10
                ((cellSize *(-5.5)), (-cellSize/2*8.75)), -- Pos 11
                ((cellSize *(-5.5)), (-cellSize/2*6.75)), -- Pos 12
                ((cellSize *(-5.5)), (-cellSize/2*4.75)), -- Pos 13
                ((cellSize *(-5.5)), (-cellSize/2*2.75)), -- Pos 14
                ((cellSize *(-5.5)), (-cellSize/2*0.75)), -- Pos 15
                ((cellSize *(-5.5)), (-cellSize/2*(-1.25))), -- Pos 16
                ((cellSize *(-5.5)), (-cellSize/2*(-3.25))), -- Pos 17
                ((cellSize *(-5.5)), (-cellSize/2*(-5.25))), -- Pos 18
                ((cellSize *(-5.5)), (-cellSize/2*(-7.25))), -- Pos 19
                ((cellSize *(-5.5)), (-cellSize/2*(-10.75))), -- Pos 20
                 ((cellSize *(-4.5)), (cellSize/2*10.75)), -- Pos 21
                ((cellSize *(-3.5)), (cellSize/2*10.75)), -- Pos 22
                ((cellSize *(-2.5)), (cellSize/2*10.75)), -- Pos 23
                ((cellSize *(-1.5)), (cellSize/2*10.75)), -- Pos 24
                ((cellSize *(-0.5)), (cellSize/2*10.75)), -- Pos 25
                ((cellSize *(0.5)), (cellSize/2*10.75)),-- Pos 26
                ((cellSize *(1.5)), (cellSize/2*10.75)),-- Pos 27
                ((cellSize *(2.5)), (cellSize/2*10.75)),-- Pos 28
                ((cellSize *(3.5)), (cellSize/2*10.75)),-- Pos 29
                ((cellSize *(4.5)), (cellSize/2*10.75)),-- Pos 30
                ((cellSize *(4.55)), (cellSize/2*7.25)), -- Pos 31
                ((cellSize *(4.55)), (cellSize/2*5.25)), -- Pos 32
                ((cellSize *(4.55)), (cellSize/2*3.25)), -- Pos 33
                ((cellSize *(4.55)), (cellSize/2*1.25)), -- Pos 34
                ((cellSize *(4.55)), (-cellSize/2*0.75)),-- Pos 35
                ((cellSize *(4.55)), (-cellSize/2*2.75)),-- Pos 36
                ((cellSize *(4.55)), (-cellSize/2*4.75)),-- Pos 37
                ((cellSize *(4.55)), (-cellSize/2*6.75)),-- Pos 38
                ((cellSize *(4.55)), (-cellSize/2*8.75))-- Pos 39

        ]

playerToken :: Player -> Color -> [Picture]
playerToken player color= drawPlayerToken (getPlayerPosition !! (boardPos player)) (playerID player) color

playersText :: [Picture]
playersText = (player1Text player1) ++ (player2Text player2) ++ (player3Text player3) ++ (player4Text player1)

player1Text :: Player -> [Picture]
player1Text player = [
                renderTxt (- widthF * 0.45) (heightF * 0.35) blue "Player",
                renderTxtSmall (- widthF * 0.45) (heightF * 0.33) white ("id: " ++ show(playerID player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.315) white ("board pos: " ++ show(boardPos player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.30) white ("chained doubles: " ++ show(chainedDoubles player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.285) white ("carteira: " ++ show(carteira player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.27) white ("deeds: " ++ show(deedsAssets player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.255) white ("isJailed: " ++ show(isJailed player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.24) white ("jailed turns: " ++ show(jailedTurns player ))
        ]

player2Text :: Player -> [Picture]
player2Text player = [
                renderTxt (- widthF * 0.45) (heightF * 0.20) green "Player",
                renderTxtSmall (- widthF * 0.45) (heightF * 0.18) white ("id: " ++ show(playerID player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.165) white ("board pos: " ++ show(boardPos player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.15) white ("chained doubles: " ++ show(chainedDoubles player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.135) white ("carteira: " ++ show(carteira player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.12) white ("deeds: " ++ show(deedsAssets player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.105) white ("isJailed: " ++ show(isJailed player )),
                renderTxtSmall (- widthF * 0.45) (heightF * 0.09) white ("jailed turns: " ++ show(jailedTurns player ))
        ]

player3Text :: Player -> [Picture]
player3Text player = [
                renderTxt (- widthF * 0.35) (heightF * 0.35) yellow "Player",
                renderTxtSmall (- widthF * 0.35) (heightF * 0.33) white ("id: " ++ show(playerID player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.315) white ("board pos: " ++ show(boardPos player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.30) white ("chained doubles: " ++ show(chainedDoubles player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.285) white ("carteira: " ++ show(carteira player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.27) white ("deeds: " ++ show(deedsAssets player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.255) white ("isJailed: " ++ show(isJailed player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.24) white ("jailed turns: " ++ show(jailedTurns player ))
        ]

player4Text :: Player -> [Picture]
player4Text player = [
                renderTxt (- widthF * 0.35) (heightF * 0.20) cyan "Player",
                renderTxtSmall (- widthF * 0.35) (heightF * 0.18) white ("id: " ++ show(playerID player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.165) white ("board pos: " ++ show(boardPos player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.15) white ("chained doubles: " ++ show(chainedDoubles player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.135) white ("carteira: " ++ show(carteira player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.12) white ("deeds: " ++ show(deedsAssets player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.105) white ("isJailed: " ++ show(isJailed player )),
                renderTxtSmall (- widthF * 0.35) (heightF * 0.09) white ("jailed turns: " ++ show(jailedTurns player ))
        ]


boardText :: [Picture]
boardText = goText ++ communityChestText ++ incomeText ++ chanceText ++ subwayText ++ jailText ++ infraText ++  freeParkText ++ goToJailText

goToJailText :: [Picture]
goToJailText = [
                renderTxtSmall ( cellSize/2 *9 + 5) (cellSize *(5) - 30) white "Go To Jail"
        ]

infraText :: [Picture]
infraText = [
                renderTxtSmall ( -cellSize/2*11 + 20) ( cellSize *(-3) + 20) white "Eletric",
                renderTxtSmall ( cellSize/2*5 + 20) ( cellSize *(5) - 35) white "Water"
        ]

freeParkText :: [Picture]
freeParkText = [
                renderTxt ( -cellSize/2 *11 ) (cellSize *(5) - 25) white "Park"
        ]

jailText :: [Picture]
jailText = [
                renderTxt ( -cellSize/2 *11 + 17) (cellSize *(-5)) white "Jail"
        ]

subwayText :: [Picture]
subwayText = [
                renderTxtSmall ( -cellSize/2*1 + 20) (-cellSize *(5) + 20) white "Subway",
                renderTxtSmall ( -cellSize/2 * 11 + 17) (cellSize *(0) + 20) white "Subway",
                renderTxtSmall ( -cellSize/2*1 + 20) ( cellSize *(5) - 35) white "Subway",
                renderTxtSmall ( cellSize/2 * 9 + 17) (cellSize *(0) + 20) white "Subway"

        ]

chanceText :: [Picture]
chanceText = [
                renderTxtSmall ( -cellSize/2 * 5 + 17) (-cellSize *(5) + 20) white "Chance",
                renderTxtSmall ( -cellSize/2 * 7 + 17) (cellSize *(5) - 35) white "Chance",
                renderTxtSmall ( cellSize/2 * 9 + 17) (cellSize *(-1) + 20) white "Chance"
        ]

incomeText :: [Picture]
incomeText = [
                renderTxtSmall ( cellSize/2 + 23) (-cellSize *(5) + 20) white "Income",
                renderTxtSmall ( cellSize/2 + 30) (-cellSize *(5) + 8) white "Tax",
                renderTxtSmall ( cellSize/2 * 9 + 23) (-cellSize *(3) + 20) white "Luxyry",
                renderTxtSmall ( cellSize/2 * 9 + 30) (-cellSize *(3) + 8) white "Tax"
        ]

goText :: [Picture]
goText = [renderTxt ( cellSize/2*9 + 15) (-cellSize *(5)) white "GO",
        renderTxtSmall ( cellSize/2*9 + 17) (-cellSize *(5) - 14) green "+200"
        ]

communityChestText :: [Picture]
communityChestText = [
                renderTxtSmall ( cellSize/2*5 + 3) (-cellSize *(5) + 20) white "Community",
                renderTxtSmall ( cellSize/2*5 + 20) (-cellSize *(5) + 8) white "Chest",
                renderTxtSmall (-cellSize/2*11 + 3) (cellSize *(2) + 20) white "Community",
                renderTxtSmall (-cellSize/2*11 + 20) (cellSize *(2) + 8) white "Chest",
                renderTxtSmall (cellSize/2*9 + 3) (cellSize *(2) + 20) white "Community",
                renderTxtSmall (cellSize/2*9 + 20) (cellSize *(2) + 8) white "Chest"
        ]

blueHouses :: [Picture]
blueHouses = [
                verticalLineColored (cellSize/2 * 9  +8) (-cellSize *(4)) (blue),
                verticalLineColored (cellSize/2 * 9  +8) (-cellSize *(2)) (blue)
        ]

greenHouses :: [Picture]
greenHouses = [
                verticalLineColored (cellSize/2 * 9  +8) (cellSize *(4)) (orange),
                verticalLineColored (cellSize/2 * 9  +8) (cellSize *(3)) (orange),
                verticalLineColored (cellSize/2 * 9  +8) (cellSize *(1)) (orange)
        ]

orangeHouses :: [Picture]
orangeHouses = [
                verticalLineColored (-cellSize/2 * 9  -8) (cellSize *(4)) (orange),
                verticalLineColored (-cellSize/2 * 9  -8) (cellSize *(3)) (orange),
                verticalLineColored (-cellSize/2 * 9  -8) (cellSize *(1)) (orange)
        ]

pinkHouses :: [Picture]
pinkHouses = [
                verticalLineColored (-cellSize/2 * 9  -8) (-cellSize *(4)) (rose),
                verticalLineColored (-cellSize/2 * 9  -8) (-cellSize *(2)) (rose),
                verticalLineColored (-cellSize/2 * 9  -8) (-cellSize *(1)) (rose)
        ]

browHouses :: [Picture]
browHouses = [
                horizontalLineColored (cellSize *(4)) (-cellSize/2 * 9  -8) (dark orange),
                horizontalLineColored (cellSize *(2)) (-cellSize/2 * 9  -8) (dark orange)
                ]
lightBlueHouses :: [Picture]
lightBlueHouses = [
                        horizontalLineColored (-cellSize *(4)) (-cellSize/2 * 9  -8) (light blue),
                        horizontalLineColored (-cellSize *(3)) (-cellSize/2 * 9  -8) (light blue),
                        horizontalLineColored (-cellSize *(1)) (-cellSize/2 * 9  -8) (light blue)
                ]
redHouses :: [Picture]
redHouses = [
                        horizontalLineColored (-cellSize *(4)) (cellSize/2 * 9  +8) (light red),
                        horizontalLineColored (-cellSize *(2)) (cellSize/2 * 9  +8) (light red),
                        horizontalLineColored (-cellSize *(1)) (cellSize/2 * 9  +8) (light red)
                ]
yellowHouses :: [Picture]
yellowHouses = [
                        horizontalLineColored (cellSize *(4)) (cellSize/2 * 9  +8) (yellow),
                        horizontalLineColored (cellSize *(2)) (cellSize/2 * 9  +8) (yellow),
                        horizontalLineColored (cellSize *(1)) (cellSize/2 * 9  +8) (yellow)
        ]

horizontalBorders :: [Picture]
horizontalBorders = [horizontalBorder (cellSize *(5.5)), horizontalBorder (cellSize *(4.5)), horizontalBorder (cellSize *(-5.5)), horizontalBorder ((cellSize *(-4.5))) ]

verticalBorders :: [Picture]
verticalBorders = [verticalBorder (cellSize *(5.5)), verticalBorder (cellSize *(4.5)), verticalBorder (-(cellSize *(4.5))) , verticalBorder (-(cellSize *(5.5))) ]




board :: Picture
board = pictures (horizontalBorders ++ verticalBorders ++ verticalLines ++ horizontalLines ++ rentHousesLines ++ boardText ++ playersVisuals ++ tileText ++ selectionVisuals )

desenhar :: Jogo -> Picture
desenhar = undefined