module Main where

import Data.List

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

type Hand = [Card]

cardValues :: Card -> [Int]
cardValues Ace = [1, 11]
cardValues Two = [2]
cardValues Three = [3]
cardValues Four = [4]
cardValues Five = [5]
cardValues Six = [6]
cardValues Seven = [7]
cardValues Eight = [8]
cardValues Nine = [9]
cardValues _ = [10] --face cards or ten have a value of ten

data Score = Value Int | Blackjack | Bust deriving (Show, Ord, Eq)

handValue :: Hand -> Score
handValue hand
	| isBlackJack hand = Blackjack
	| (length under21) == 0 = Bust
	| otherwise = Value (maximum under21)
	where 
		isBlackJack :: Hand -> Bool
		isBlackJack [card1, card2] = 
			card1 == Ace && card2 `elem` [Ten, Jack, Queen, King] ||
			card2 == Ace && card1 `elem` [Ten, Jack, Queen, King]
		isBlackJack _ = False
		possibleHandTotals :: Hand -> [Int] -> [Int]
		possibleHandTotals [] totals = sort $ nub $ totals
		possibleHandTotals (card:cards) runningTotals = possibleHandTotals cards newTotals
			where newTotals = [possibleCardValue + currTotal | possibleCardValue <- cardValues card, currTotal <- runningTotals]
		under21 = filter (<=21) (possibleHandTotals hand [0])

data Move = Hit | Stand deriving (Show, Eq) -- won't support doubling down yet
data GameState = PlayerPlaying | DealerPlaying
data Outcome = Loss | Push | Win deriving (Show, Eq) -- won't support blackjack wins yet

dealerNextMove :: Hand -> Move
dealerNextMove hand
	| value <= Value 17 = Hit
	| otherwise = Stand
	where value = handValue hand

playerNextMove :: String -> Move
playerNextMove "hit" = Hit
playerNextMove _   = Stand

hand = [Two, Ace]

main = do
	move <- getLine
	print $ parseMove move






