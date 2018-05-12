{-# LANGUAGE MultiWayIf #-}
module Utility
  ( yesNo
  ) where

import           Data.Char (toLower)

yesNo :: String -> IO Bool
yesNo question = do
  putStrLn $ question ++ " (Y/n)"
  answer <- getLine
  if
    | isYes answer -> putStrLn "" >> return True
    | isNo  answer -> putStrLn "" >> return False
    | otherwise    -> yesNo question
  where
    isYes s = null s || map toLower s `elem` ["y", "yes"]
    isNo  s = map toLower s `elem` ["n", "no"]
