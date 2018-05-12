module Card
  ( Card
  , deck
  , displayCard
  , toPoint
  ) where

import           Data.List.NonEmpty (NonEmpty, fromList)

data Card = A | N Int | J | Q | K
  deriving (Show)

suit, heartSuit, diaSuit, cloverSuit, spadeSuit :: [Card]
suit = [A] ++ map N [2..10] ++ [J, Q, K]

heartSuit  = suit
diaSuit    = suit
cloverSuit = suit
spadeSuit  = suit

deck :: [Card]
deck = heartSuit ++ diaSuit ++ cloverSuit ++ spadeSuit

displayCard :: Card -> String
displayCard (N n) = show n
displayCard c     = show c

toPoint :: Card -> NonEmpty Int
toPoint A     = fromList [1, 11]
toPoint (N n) = pure n
toPoint _     = pure 10
