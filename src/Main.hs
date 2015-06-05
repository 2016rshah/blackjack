module Main where

import Data.List

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

type Hand = [Card]
type Deck = [Card]

fullDeck :: Deck
fullDeck = [Two .. Ace] ++ [Two .. Ace] ++ [Two .. Ace] ++ [Two .. Ace]

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

--playerNextMove :: String -> Move
--playerNextMove "hit" = Hit
--playerNextMove _   = 
playerNextMove :: Hand -> Move
playerNextMove playerHand
    | playerScore <= Value 17   = Hit
    | otherwise                 = Stand
    where playerScore = handValue playerHand

determineWinner :: Score -> Score -> Outcome
determineWinner playerScore dealerScore 
    | playerScore < dealerScore = Loss
    | playerScore > dealerScore = Win
    | otherwise                 = Push

roundOutcome :: GameState -> Hand -> Hand -> Deck -> Outcome 
roundOutcome PlayerPlaying playerHand dealerHand deck@(card:cards) 
    | playerScore == Bust = roundOutcome DealerPlaying playerHand dealerHand deck
    | playerMove == Stand = roundOutcome DealerPlaying playerHand dealerHand deck
    | playerMove == Hit   = roundOutcome DealerPlaying (card:playerHand) dealerHand cards
    where   playerScore = handValue playerHand
            playerMove = playerNextMove playerHand
roundOutcome DealerPlaying playerHand dealerHand deck@(card:cards)
    | dealerScore == Bust   = determineWinner playerScore dealerScore
    | dealerMove  == Stand  = determineWinner playerScore dealerScore
    | dealerMove  == Hit    = roundOutcome PlayerPlaying playerHand (card:dealerHand) cards 
    where   playerScore = handValue playerHand
            dealerScore = handValue dealerHand
            dealerMove  = dealerNextMove dealerHand

hand = [Two, Ace]

main = do
    move <- getLine
    print $ "Nothing is interactive yet"






