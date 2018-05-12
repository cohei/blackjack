{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Hand
  ( Hand
  , addCard
  , addCards
  , displayHand
  , isBusted
  , doesDealerFinish
  , judge
  ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE (filter)
import           Data.Ord           (Ordering (EQ, GT, LT), comparing)

import           Card               (Card, displayCard, toPoint)
import           Result             (Reason (Point), Result (Draw, Lose, Win))

newtype Hand = Hand { cards :: [Card] }
  deriving (Monoid)

addCard :: Card -> Hand -> Hand
addCard c = Hand . (c :) . cards

addCards :: Traversable t => t Card -> Hand -> Hand
addCards = flip $ foldr addCard

toPoints :: Hand -> NonEmpty Int
toPoints = fmap sum . mapM toPoint . cards

displayHand :: Bool -> Hand -> String
displayHand _            (Hand []) = ""
displayHand showHoleCard (Hand (reverse -> holeCard : cs)) =
  unwords $ (if showHoleCard then displayCard holeCard else "_") : map displayCard cs

isBusted :: Hand -> Bool
isBusted = all (> bustPoint) . toPoints

doesDealerFinish :: Hand -> Bool
doesDealerFinish = any (>= 17) . NE.filter (<= bustPoint) . toPoints

finalPoint :: Hand -> Int
finalPoint = maximum . NE.filter (<= bustPoint) . toPoints

judge :: Hand -> Hand -> Result
judge player dealer =
  case comparing (abs . subtract bustPoint . finalPoint) player dealer of
    LT -> Win Point
    EQ -> Draw
    GT -> Lose Point

bustPoint :: Int
bustPoint = 21
