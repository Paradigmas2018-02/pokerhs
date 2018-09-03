module Player (
    Player(..),
    bet,
    charge,
) where

data Player = Player { name :: String, tokens :: Int} deriving (Show, Eq)


-- Make a bet to the game --
--      >> b - value of the bet
--      >> p - actual pot of tokens
bet :: Int -> Int -> Int
bet b p =  p + b


-- Charge a bet value from the player --
--      >> p - player that will be charged
--      >> b - the value of the charge
charge :: Player -> Int -> Player
charge p b = 
    if tokens p < b
        then Player {name = name p, tokens = 0}
    else
        Player {name = name p, tokens = tokens p - b}
