{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Web.Scotty
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import Poker(flop, giveCards, Table(..), Hand(..))
import Deck(Card(..), Rank(..), deck, remainingDeck, pick)
import Player(Player(..), bet)
import Utils(destroyTuples, destroyTuple)
import Find(findHand)

data Game = Game {player1 :: Player, player2 :: Player, table :: Table} deriving(Generic, Show)
instance ToJSON Game

data Bet = Bet {player :: String, value :: Int} deriving(Generic, Show)
instance FromJSON Bet

data Winner = Winner { winnerPlayer :: Player, winnerHand :: Hand, loserHand :: Hand } deriving(Generic, Show)
instance ToJSON Winner

main = do
    mgame <- newGame
    game <- newMVar mgame

    scotty 3000 $ do
        get "/deck" $ json deck

        get "/winner" $ do
            g <- liftIO $ readMVar game
            json $ winner g

        get "/newgame" $ do
            g <- liftIO newGame
            liftIO $ modifyMVar game $ \game' -> return (g, True)
            json g
        
        get "/pick" $ do
            g <- liftIO $ readMVar game
            c <- liftIO $ pick $ remainingDeck (tcards (table g) ++ destroyTuple (cards $ player1 g) ++ destroyTuple (cards $ player2 g))
            liftIO $ modifyMVar game $ \game' ->
                return (Game {player1= player1 g, player2 = player2 g, table = Table {tcards = c : tcards (table g), pot = pot $ table g, thand = findHand $ c : tcards (table g)}}, True)
            ng <- liftIO $ readMVar game
            json ng
           
        get "/game" $ do
            g <- liftIO $ readMVar game
            json g

        get "/newturn" $ do
            g <- liftIO $ readMVar game
            nt <- liftIO $ newTurn g
            liftIO $ modifyMVar game $ \game' -> return (nt, True)
            ng <- liftIO $ readMVar game
            json ng
            
        post "/bet" $ do
            b <- jsonData
            g <- liftIO $ readMVar game
            
            case player b of
                "player1" -> 
                    liftIO $ modifyMVar game $ \game' ->
                        return (Game {
                            player1 = fst (bet (player1 g) (value b) (pot $ table g)),
                            player2 = player2 g, 
                            table = Table {
                                tcards = tcards $ table g,
                                pot = snd (bet (player1 g) (value b) (pot $ table g)),
                                thand = thand $ table g
                                }
                        }, True)
                "player2" ->
                    liftIO $ modifyMVar game $ \game' ->
                        return (Game {
                            player1 = player1 g,
                            player2 = fst (bet (player2 g) (value b) (pot $ table g)),
                            table = Table {
                                tcards = tcards $ table g,
                                pot = snd (bet (player2 g) (value b) (pot $ table g)),
                                thand = findHand $ tcards (table g)
                                }
                        }, True)

            ng <- liftIO $ readMVar game
            json ng


newGame :: IO Game
newGame = do 
    xs <- giveCards deck 2
    f <- flop (remainingDeck (destroyTuples xs))
    return 
        Game {
            player1 = Player {name="Bot", tokens=200, cards=head xs, phand=findHand $ f ++ destroyTuple (head xs), payment = 0},
            player2 = Player {name="Human", tokens=200, cards=xs !! 1, phand=findHand $ f ++ destroyTuple (xs !! 1), payment = 0},
            table = Table {tcards = f, pot = 0, thand = findHand f}
        }

newTurn :: Game -> IO Game
newTurn g = do
    xs <- giveCards deck 2
    f <- flop (remainingDeck (destroyTuples xs))
    return
        Game {
            player1 = Player {
                name = "Bot",
                tokens = tokens $ player1 g,
                cards = head xs,
                phand = findHand $ f ++ destroyTuple (head xs),
                payment = 0
            },
            player2 = Player {
                name = "Human",
                tokens = tokens $ player2 g,
                cards = xs !! 1,
                phand = findHand $ f ++ destroyTuple (xs !! 1),
                payment = 0
            },
            table = Table {tcards = f, pot = 0, thand = findHand f}
        }

winner :: Game -> Winner
winner g =
    if (hvalue $ findHand (aux (player1 g) ++ tcards (table g))) >
        (hvalue $ findHand (aux (player2 g) ++ tcards (table g)))
        then 
            Winner {winnerPlayer = player1 g, winnerHand = findHand (aux (player1 g) ++ tcards (table g)), loserHand = findHand (aux (player2 g) ++ tcards (table g))}
    else
        Winner {winnerPlayer = player2 g, winnerHand = findHand (aux (player2 g) ++ tcards (table g)), loserHand = findHand (aux (player1 g) ++ tcards (table g))}


aux :: Player -> [Card]
aux p =
    destroyTuple (cards p)