module Message where

import TileType

data Message = Message {
                mainmessage :: String,
                answeroptions :: [String]
            }

justClose :: [String]
justClose = ["Press c to close this warning"]

makeBuyOffer :: RealTile -> Message
makeBuyOffer rt@(NBTile y) = Message buy opL
  where
    buy = "Do you wish to buy "++nameNB y++" for $"++(show.price) y++"?"
    opL = ["Press y to buy", "Press n to not buy"{-, "Press a to start an Auction"-}]
makeBuyOffer rt@(LTile y) = Message buy opL
  where
    buy = "Do you wish to buy "++nameB y++" for $"++(show.price) y++"?"
    opL = ["Press y to buy", "Press n to not buy"{-, "Press a to start an Auction"-}]
makeBuyOffer (MTile _) = error "misc tile being offered"

freeRoamMessage :: Int -> Message
freeRoamMessage n = Message rM rC
  where
    rM = "Player "++show n++", you are free to explore the board"
    rC = ["Press the arrows Up or Down to move the cursor"
        , "Press r to roll die and start turn"
        , "Press u to upgrade or unmortgage the current cursor's tile"
        , "Press d to downgrade the current cursor's tile and get money back"
        , "Press m to sell upgrades and mortgage the current cursor's tile to get money back"
        , "Press q to declare bankrupcy"]

failedOperation :: Int -> Message
failedOperation n = Message fM justClose
  where
    fM = "Player "++show n++", you lack ownership or funds to operate"

failedToLeaveJail :: Int -> Int -> Message
failedToLeaveJail n d = Message fM justClose
  where
    fM = "Player "++show n++" failed to escape from jail,\n will be released in "++show (3-d)++" turns"

goToJailMessage :: Int -> Bool -> Int -> Message
goToJailMessage n b rng = Message jM jC
  where
    base = "Player "++show n
    onfire 
      | b = " recent actions has attracted federal attention"
      | otherwise = ""
    charge = charges !! (rng `mod` length charges)
    arrestfor = "You have been arrested for:"
    serve = "go to jail and serve thy sentence"
    jM = base ++ onfire
    jC = arrestfor:charge:serve:justClose

charges :: [String] -- no maximo ate 16 (pra usarmos o rng do chance) MAXIMO ALCANCADO
charges = [
            "Being simply unlucky",
            "Being just too good at this game",
            "Kicking a puppy",
            "A crypto scam",
            "Illicit drug possession",
            "Developing software on Windows",
            "Cheating at this game",
            "Tax evasion",
            "Posting hate speech on Twitter ...... I mean X",
            "Copyright infringement",
            "Being framed for another player's crime",
            "Not... getting away with murder",
            "Not appreciating Free Parking",
            "Breaking the game with their last git push",
            "Whatever reason really, jail cell rent is FREE",
            "Not sharing their snacks"
        ]

jailProposalMessage :: Int -> Message
jailProposalMessage n = Message pM pC
  where
    pM = "Crime doesn't pay, Player "++show n++", ... for itself"
    pC = ["Press y to pay a $50 fine and leave jail"
          , "Press n to attempt a jailbreak by rolling doubles!"
          ]

alreadyMaxLevelMessage :: Int -> Message
alreadyMaxLevelMessage n = Message mM justClose
  where
    mM = "Player "++show n++", this land is already at max level!"

perhapsMorgageMessage :: Int -> Message
perhapsMorgageMessage n = Message mM ("Press m to sell upgrades and mortgage the current cursor's tile to get money back":justClose)
  where
    mM = "Player "++show n++", perhaps you might have meant to mortgage this lot"

alreadyMortgaged :: Int -> Message
alreadyMortgaged n = Message mM justClose
  where
    mM = "Player "++show n++", this lot is already mortgaged"