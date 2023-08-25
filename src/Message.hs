module Message where

import TileType

data Message = Message {
                mainmessage :: String,
                answeroptions :: [String]
            }

makeBuyOffer :: RealTile -> Message
makeBuyOffer rt@(NBTile y) = Message buy opL
  where
    buy = "Do you wish to buy "++nameNB y++" for $"++(show.price) y++"?"
    opL = ["Press b to buy", "Press n to not buy"{-, "Press a to start an Auction"-}]
makeBuyOffer rt@(LTile y) = Message buy opL
  where
    buy = "Do you wish to buy "++nameB y++" for $"++(show.price) y++"?"
    opL = ["Press b to buy", "Press n to not buy"{-, "Press a to start an Auction"-}]
makeBuyOffer (MTile _) = error "misc tile being offered"

freeRoamMessage :: Int -> Message
freeRoamMessage n = Message rM rC
  where
    rM = "Player "++show n++", you are free to explore the board"
    rC = ["Press the arrows Up or Down to move the cursor"
        , "Press r to roll die and start turn"
        , "Press u to upgrade or unmortgage the current cursor's tile"
        , "Press d to downgrade the current cursor's tile"
        , "Press m to mortgage the current cursor's tile"]

failedUpgrade :: Int -> Message
failedUpgrade n = Message fM fC
  where
    fM = "Player "++show n++", you lack ownership or funds to upgrade this tile"
    fC = ["Press c to close this warning"]

failedToLeaveJail :: Int -> Int -> Message
failedToLeaveJail n d = Message fM fC
  where
    fM = "Player "++show n++" failed to escape from jail, will be released in "++show (3-d)++" turns"
    fC = ["Press c to close this window"]

goToJail :: Int -> Bool -> Int -> Message
goToJail n b rng = Message jM jC
  where
    jC = ["Press c to close this window"]
    base = "Player "++show n
    onfire 
      | b = " recent actions has attracted federal attention"
      | otherwise = ""
    charge = charges !! (rng `mod` length charges)
    arrest = "\n You have been arrested for:\n"++charge++"\n go to jail and serve thy sentence"
    jM = base ++ onfire ++ arrest

charges :: [String]
charges = [
            "Kicking a puppy",
            "A crypto scam"
        ]