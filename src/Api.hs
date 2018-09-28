{-# LANGUAGE
    OverloadedStrings,
    DeriveGeneric
#-}

import GHC.Generics
import Data.Aeson (ToJSON)
import Web.Scotty
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Poker(flop, giveCards, Table(..))
import Deck(Card, deck, remainingDeck, pick)
import Player(Player(..))
import Utils(destroyTuples, destroyTuple)
import Find(findHand)

data Game = Game {player1 :: Player, player2 :: Player, table :: Table} deriving(Generic, Show)
instance ToJSON Game


main = do
    game <- newMVar newGame

    scotty 3000 $ do
        get "/deck" $ json deck

        get "/newgame" $ do
            g <- liftIO newGame
            liftIO $ modifyMVar game $ \game' -> return (newGame, True)
            json g
        
        -- get "/pick" $ do
        --     var <- liftIO $ readMVar game
        --     g <- liftIO var
        --     c <- liftIO $ pick (tcards $ table g ++ destroyTuple (cards $ player1 g) ++ destroyTuple (cards $ player2 g))
        --     json c
           
        -- post "/bet" text "Not Implemented"


newGame :: IO Game
newGame = do 
    xs <- giveCards deck 2
    f <- flop (remainingDeck (destroyTuples xs))
    return 
        Game {
            player1 = Player {name="Bot", tokens=200, cards=head xs, phand=findHand $ f ++ destroyTuple (head xs)},
            player2 = Player {name="Human", tokens=200, cards=xs !! 1, phand=findHand $ f ++ destroyTuple (xs !! 1)},
            table = Table {tcards = f, pot = 0, thand = findHand f}
        }
