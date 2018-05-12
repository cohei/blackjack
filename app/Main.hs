module Main where

import           Control.Monad.State (evalStateT)

import           Game                (game, initialGame)
import           Result              (displayResult)

main :: IO ()
main = putStrLn . displayResult =<< evalStateT game =<< initialGame
