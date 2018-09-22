{-# LANGUAGE
    OverloadedStrings,
    DeriveGeneric
#-}

import GHC.Generics
import Data.Aeson (ToJSON)
import Web.Scotty
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Poker(flop, giveCards)
import Deck(Card, deck, remainingDeck, pick)
import Player(Player(..))
import Utils(destroyTuples, destroyTuple)

data Game = Game {player1 :: Player, player2 :: Player, gflop :: [Card]} deriving(Generic, Show)
instance ToJSON Game


main = do
    game <- newMVar newGame

    scotty 3000 $ do
        get "/deck" $ json deck

        get "/newgame" $ do
            g <- liftIO newGame
            liftIO $ modifyMVar game $ \game' -> return (game', True)
            json g
        
        get "/pick" $ do
            var <- liftIO $ readMVar game
            g <- liftIO var
            c <- liftIO $ pick (gflop g ++ destroyTuple (cards $ player1 g) ++ destroyTuple (cards $ player2 g))
            json c


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
