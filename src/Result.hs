module Result
  ( Result(Lose, Draw, Win)
  , Reason (Bust, Point)
  , displayResult
  ) where

data Result = Lose Reason | Draw | Win Reason
  deriving (Show)

data Reason = Bust | Point
  deriving (Show)

displayResult :: Result -> String
displayResult (Lose Bust)  = "Lose by player bust"
displayResult (Lose Point) = "Lose by point"
displayResult Draw         = show Draw
displayResult (Win Bust)   = "Win by dealer bust"
displayResult (Win Point)  = "Win by point"
