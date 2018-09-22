{-# LANGUAGE
    OverloadedStrings,
    DeriveGeneric
#-}

import GHC.Generics
import Data.Aeson (ToJSON)
import Web.Scotty
import Control.Monad.IO.Class
import Poker(flop, giveCards)
import Deck(Card, deck, remainingDeck)
import Player(Player(..))
import Utils(destroyTuples)

data Game = Game {player1 :: Player, player2 :: Player, gflop :: [Card]} deriving(Generic, Show)
instance ToJSON Game


main =
    scotty 3000 $ do
        get "/deck" $ json deck

        get "/newgame" $ do
            game <- liftIO newGame
            json game


newGame :: IO Game
newGame = do 
    xs <- giveCards deck 2
    f <- flop (remainingDeck (destroyTuples xs))
    return 
        Game {
            player1 = Player {name="Bot", tokens=200, cards=head xs},
            player2 = Player {name="Human", tokens=200, cards=xs !! 1},
            gflop = f
        }
