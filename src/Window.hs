module Window where

import Graphics.Gloss

type Line = (Float, Float, Float) -- (x, y, size)

-- Largura
width :: Int 
width = 1920

-- Altura
height :: Int
height = 1080

-- Cor de fundo
background :: Color
background = black

-- Criar Janela
window = InWindow "Monopoli" (width, height) (0 , 0)

fps :: Int 
fps = 30

lineColor :: Color
lineColor = white


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
board = pictures (horizontalBorders ++ verticalBorders ++ verticalLines ++ horizontalLines ++ rentHousesLines ++ boardText)

