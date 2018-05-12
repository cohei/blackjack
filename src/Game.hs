{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Game
  ( initialGame
  , game
  ) where

import           Control.Lens           (Lens', makeLenses, use, uses, (%=),
                                         (.=))
import           Control.Monad          (replicateM_)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Control.Monad.Random   (MonadRandom)
import           Control.Monad.State    (MonadState (get))
import           System.Random.Shuffle  (shuffleM)

import           Card                   (Card, deck)
import           Hand                   (Hand, addCard, displayHand,
                                         doesDealerFinish, isBusted, judge)
import           Result                 (Reason (Bust), Result (Lose, Win))
import           Utility                (yesNo)

data Game = Game
  { _stock        :: [Card]
  , _dealerHand   :: Hand
  , _playerHand   :: Hand
  , _showHoleCard :: Bool
  }
makeLenses ''Game

initialGame :: MonadRandom m => m Game
initialGame = do
  shuffled <- shuffleM deck
  return Game
    { _stock = shuffled
    , _dealerHand = mempty
    , _playerHand = mempty
    , _showHoleCard = False
    }

draw :: MonadState Game m => m Card
draw = do
  c <- uses stock head
  stock %= tail
  return c

drawToHand :: (MonadIO m, MonadState Game m) => Lens' Game Hand -> m Bool
drawToHand l = do
  c <- draw
  l %= addCard c
  display
  uses l isBusted

firstDeal :: (MonadIO m, MonadState Game m) => m ()
firstDeal = do
  replicateM_ 2 $ drawToHand dealerHand
  replicateM_ 2 $ drawToHand playerHand

playerDraw :: (MonadIO m, MonadState Game m) => m Bool
playerDraw = drawToHand playerHand

dealerDraw :: (MonadIO m, MonadState Game m) => m Bool
dealerDraw = drawToHand dealerHand

displayGame :: Game -> String
displayGame Game {..} = unlines
  [ ("Dealer: " ++) $ displayHand _showHoleCard _dealerHand
  , ("Player: " ++) $ displayHand True _playerHand
  ]

display :: (MonadIO m, MonadState Game m) => m ()
display = liftIO . putStr . (++ "\n") . displayGame =<< get

playerTurns :: (MonadIO m, MonadState Game m) => m Bool
playerTurns = do
  hit <- liftIO $ yesNo "Hit?"

  if not hit
    then return False
    else do
      busted <- playerDraw

      if busted
        then return True
        else playerTurns

dealerTurns :: (MonadIO m, MonadState Game m) => m Bool
dealerTurns = do
  showHoleCard .= True
  display
  loop
  where
    loop = do
      finish <- uses dealerHand doesDealerFinish

      if finish
        then return False
        else do
          busted <- dealerDraw

          if busted
            then return True
            else loop

game :: (MonadIO m, MonadState Game m) => m Result
game = do
  firstDeal

  playerBusted <- playerTurns

  if playerBusted
    then return $ Lose Bust
    else do
      dealerBusted <- dealerTurns

      if dealerBusted
        then return $ Win Bust
        else judge <$> use playerHand <*> use dealerHand
