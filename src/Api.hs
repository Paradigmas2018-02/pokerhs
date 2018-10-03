{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import GHC.Generics
import Data.Aeson (ToJSON)
import Web.Scotty
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Poker(flop, giveCards, Table(..))
import Deck(Card(..), Rank(..), deck, remainingDeck, pick)
import Player(Player(..), bet)
import Utils(destroyTuples, destroyTuple)
import Find(findHand)

data Game = Game {player1 :: Player, player2 :: Player, table :: Table} deriving(Generic, Show)
instance ToJSON Game


main = do
    mgame <- newGame
    game <- newMVar mgame

    scotty 3000 $ do
        get "/deck" $ json deck

        get "/newgame" $ do
            g <- liftIO newGame
            liftIO $ modifyMVar game $ \game' -> return (g, True)
            json g
        
        get "/pick" $ do
            g <- liftIO $ readMVar game
            c <- liftIO $ pick $ remainingDeck (tcards (table g) ++ destroyTuple (cards $ player1 g) ++ destroyTuple (cards $ player2 g))
            liftIO $ modifyMVar game $ \game' ->
                return (Game {player1= player1 g, player2 = player2 g, table = Table {tcards = c : tcards (table g), pot = pot $ table g, thand = findHand $ c : tcards (table g)}}, True)
            json c
           
        get "/game" $ do
            g <- liftIO $ readMVar game
            json g
    
        post "/bet" $ do
            value <- param "value"
            g <- liftIO $ readMVar game
            liftIO $ modifyMVar game $ \game' ->
                return (Game {player1= fst (bet (player1 g) value (pot $ table g)), player2 = player2 g, table = Table {tcards = tcards $ table g, pot = snd ( bet (player1 g) value (pot $ table g) ), thand = findHand $ tcards (table g)}}, True)
            json g


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
