{-# LANGUAGE DeriveGeneric #-}

module Poker (
    flop,
    -- newRoles,
    giveCards,
    Hand(..),
) where

import GHC.Generics
import Data.Aeson (ToJSON)
import Deck (Card,Rank, pick, remainingCards)
import Utils(development, createTuples)
import System.Process
import System.IO
import Data.Char
import Control.Monad

-- A hand in a texas holden poker --
data Hand = Hand { hvalue :: Rank, hcards :: [Card]} deriving (Generic, Show, Eq, Ord)
instance ToJSON Hand

clear = system "clear";
betMessage = "bet";
coverMessage = "covered";
outMessage = "run out";
checkMessage = "pass";


flop :: [Card] -> IO [Card]
flop xs =
    core xs []
    where core xs c_acc = do
            c <- pick xs
            if length c_acc == 3
                then return c_acc
            else
                core (remainingCards xs (c:c_acc)) (c:c_acc)


-- Define the new big blind and the new small blind --
-- We treat the player list as a stack e.g [p1, p2, p3]
-- where p1 and p2 are big and small blind respectively.
-- If newRoles is called the new stack is [p2, p3, p1]
-- where p2 and p3 are big and small blind respectively.
-- newRoles :: [Player] -> [Player]
-- newRoles xs = tail xs ++ [head xs]

-- showActions :: String -> (Player,Int) -> IO()
-- showActions acao (x, y) =
--   case acao of
--     "1" -> putStrLn ("Player: " ++ name x ++ " " ++ betMessage)
--     "2" -> putStrLn ("Player: " ++ name x ++ " " ++ coverMessage)
--     "3" -> putStrLn ("Player: " ++ name x ++ " " ++ outMessage)
--     "4" -> putStrLn ("Player: " ++ name x ++ " " ++ checkMessage)

  -- Player's Turn --
  --      >> player - player who is playing
  --      >> betValue - actual value of the bet
-- turn :: Player -> Int -> IO()
-- turn player betValue= do
--   clear
--   putStrLn "=== SEU TURNO ===\n";
--   putStrLn "Bet: "
--   print betValue;
--   putStrLn "Choose an action\n";
--   putStrLn "1 - Bet\n";
--   putStrLn "2 - Cover\n";
--   putStrLn "3 - Out\n";
--   putStrLn "4 - Pass\n";
--   option <- getLine;
--   case option of
--     "1" -> showActions "1" (bet player 0 0);
--     "2" -> showActions "2" (bet player 0 0);
--     "3" -> showActions "3" (bet player 0 0);
--     "4" -> showActions "4" (bet player 0 0);


-- -- Function test to execute the game
-- testMain = do
--   xs <- playerCards
--   let player = Player { name="You", tokens=initialTokens, cards=xs !! 5};
--   turn player 0;

-- Give cards for n players --
--      >> xs - a list of available cards
--      >> n - the number of players
giveCards :: [Card] -> Int -> IO [(Card, Card)]
giveCards xs n =
    core xs (n*2) []
    where core xs n c_acc = do
            c <- pick xs
            if n == 0
                then return (createTuples c_acc)
            else
                core (remaining xs c_acc c) (n-1) (c:c_acc)

          remaining xs ys e = [x | x <- xs, x /= e, e `notElem` ys]