module Poker (
    flop,
) where

import Deck (Card, pick, remainingCards)
import System.Process
import System.IO
import Data.Char
import Control.Monad

clear = system "clear";
bet = "apostou";
cover = "cobriu a aposta";
out = "fugiu";
check = "passou";


flop :: IO [Card] -> IO [Card]
flop xs = do
    ys <- xs
    core ys []
    where core xs c_acc = do
            c <- pick xs
            if length c_acc == 3
                then return c_acc
            else
                core (remainingCards xs (c:c_acc)) (c:c_acc)

development :: IO()
development = do
  clear
  putStrLn "Essa feature está sendo desenvolvida. Por favor aguarde novas atualizações do sistema";

-- showActions :: [IO] -> IO()
-- showActions xs = do
--   if ((head xs) == "1") then (putStrLn bet)

-- opcao bot1, opcao bot2, opcao bot3, opcao bot 4, opcao bot 5, valorApostaAtual,

-- valor da aposta ( Começa com o valor inicial )
turn :: Int -> IO()
turn betValue = do
  clear
  putStrLn "=== SEU TURNO ===\n";
  putStrLn "Valor Aposta: "
  putStrLn betValue
  putStrLn "Escolha uma ação\n";
  putStrLn "1 - Apostar\n";
  putStrLn "2 - Cobrir apostar\n";
  putStrLn "3 - Fugir\n";
  putStrLn "4 - Passar\n";
  option <- getLine;
  case option of
    "1" -> development
    "2" -> development
    "3" -> development
    "4" -> development
