{-| Author: Xiuge Chen <xiugec@student.unimelb.edu.au>
    Created on: 2020.03.25
    Last Modified: 2020.03.29

    Purpose: Mimic a simplified Battleship game played on 4×8 grid with two 
    players, a searcher and a hider, where the searcher will try to find the 
    locations of three battleships hidden by the hider using as few as 
    possible number of guesses.
      
    To help the searching process, hider will provide following additional 
    information in response to each guess.
      1. number of guesses exactly located at a ship
      2. number of guesses that were exactly one space away from a ship
      3. number of guesses that were exactly two spaces away from a ship

    Search Strategies: always choose the guess with the smallest expected 
    number of remaining possible targets.

    Main Data:
    1. Location: grid location on the board
    2. GameState: the helper game state maintained by the searcher

    Main Functions:
    1. toLocation: covert the string representation of location to data
    2. feedback: feedback given by hider as described above
    3. initialGuess: the first guess that the searcher will make
    4. nextGuess: general guessing after the initial guess.
-}

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List

-- **************** Data Definition **************** --

-- Possible row number of the board
-- Note: Order does matter, which represents the distance between rows
data Row = One | Two | Three | Four
    deriving (Eq, Ord, Enum)

instance Show Row where 
    show One   = "1"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"

-- Possible column number of the board
-- Note: Order does matter, which represents the distance between columns
data Col = A | B | C | D | E | F | G | H
    deriving (Eq, Ord, Show, Enum)

-- A grid location of the board
data Location = Location Col Row 
    deriving (Eq, Ord)

instance Show Location where 
    show (Location col row) = show col ++ show row

-- Feedback, a 3-tuple as described above
type Feedback = (Int, Int, Int)

-- A guess or a target
type Targets = [Location]
type Guesses = [Location]

-- The state of game, including all remaining possible targets
-- get updates after each feedback
type GameState = [Targets]

-- **************** Constants **************** -- 

-- Number of target in this game
numTargets :: Int
numTargets = 3

-- The first Guess that the searcher will make, hard-coded by choosing 
-- the one that has smallest expected remaining targets
firstGuess :: Guesses
firstGuess = [ (Location A Four), (Location H Two), (Location H Four) ]

-- All possible locations (grids) on the board
allLocs :: [Location]
allLocs = [ Location col row | col <- [A ..], row <- [One ..] ]

-- Initial game state, simply just make every grid a potential target
initGameState :: GameState
initGameState = subseqLenK numTargets allLocs

-- **************** Functions **************** --

-- Generate all subseqences of a list that is of length k
subseqLenK :: Int -> [a] -> [[a]]
subseqLenK 0 _  = [ [] ]
subseqLenK k xs = [ y:zs | (y:ys) <- tails xs, zs <- subseqLenK (k-1) ys]

-- Convert a string to Row data
-- Input must be valid
toRow :: String -> Row
toRow r
    | r == "1"  = One
    | r == "2"  = Two
    | r == "3"  = Three
    | r == "4"  = Four
    | otherwise = error "Undefined string representation of row"

-- Convert a string to Col data
-- Input must be valid
toCol :: String -> Col
toCol c
    | c == "A"  = A
    | c == "B"  = B
    | c == "C"  = C
    | c == "D"  = D
    | c == "E"  = E
    | c == "F"  = F
    | c == "G"  = G
    | c == "H"  = H
    | otherwise = error "Undefined string representation of column"

-- Convert a String to Location data, Nothing returned if input invalid
toLocation :: String -> Maybe Location
toLocation s =
    if length s == 2
        then Just (Location (toCol(take 1 s)) (toRow(drop 1 s)) )
    else Nothing

-- Calculate the grid distance between two locations
-- Assume row/col distance is refected from their enum order when defined
distLocs :: Location -> Location -> Int
distLocs (Location c1 r1) (Location c2 r2)
    = max distCol distRow
    where distCol = abs (fromEnum c1 - fromEnum c2)
          distRow = abs (fromEnum r1 - fromEnum r2)

-- Get the distance between single guess location and the closest targets.
-- Assumed the maximum distance is smaller than 99
minDistGusTgs :: Location -> Targets -> Int
minDistGusTgs _ []         = 99 -- must be a suficient large number
minDistGusTgs gus (tg:tgs) = min (distLocs gus tg) (minDistGusTgs gus tgs)

-- Give the 3-tuple feedback of a pair of targets and guesses, as described
-- in the header.
-- Assumed the first argument is the targets and the second is the guesses
feedback :: Targets -> Guesses -> (Int, Int, Int)
feedback _ []           = (0, 0, 0)
feedback tgs (gus:guses) 
    | gusFedbk == 0 = (a+1, b, c)
    | gusFedbk == 1 = (a, b+1, c)
    | gusFedbk == 2 = (a, b, c+1)
    | otherwise     = (a, b, c)
    where gusFedbk = minDistGusTgs gus tgs
          (a,b,c)  = feedback tgs guses

-- The first (most optimized) guess that the searcher will make
initialGuess :: (Guesses, GameState)
initialGuess = (firstGuess, initGameState)

-- Update game state based on the previous guesses and recieved feedback
-- eliminate all targets that are not possible any more (i.e. previously
-- guessed or has feedback violates the recieved feedback)
updateGameState :: GameState -> Guesses -> Feedback -> GameState
updateGameState tgs gus fb = 
    [ tg | tg <- tgs, feedback tg gus == fb && sort gus /= sort tg ]

-- Calculate the expected number of remaining targets if make this guess
expRemainTgs :: Guesses -> GameState -> Double
expRemainTgs gus tgs = sum (map (\x -> x * x / totalNumTgs) numTgsGrouply)
    where totalNumTgs   = fromIntegral (length tgs)
          numTgsGrouply = map (fromIntegral.length) fbGroup
          -- group targets according to their feedback with gus
          fbGroup       = group (sort (map (`feedback` gus) tgs) )

-- Choose the best guesses with lowest expected number of remaining targets
-- Assumed the first argument is used for iterating all possible targets 
-- and the second is holding all of them
-- Also assumed the largest expected number is smaller than 9999
bestGuess :: GameState -> GameState -> (Guesses, Double)
bestGuess [] tgs = ([], 9999) -- sufficient large expected number
bestGuess (gus:guses) tgs = 
    if gusExpTgs < snd bestExpTgs
        then (gus, gusExpTgs)
    else bestExpTgs
    where gusExpTgs  = expRemainTgs gus tgs
          bestExpTgs = bestGuess guses tgs

-- Make next guess based on previous guess, the feedback to that, and the
-- game state before recieving this feedback.
nextGuess :: (Guesses, GameState) -> Feedback -> (Guesses, GameState)
nextGuess (preGuses, tgs) fb = 
    (fst (bestGuess updatedTgs updatedTgs), updatedTgs)
    where updatedTgs = updateGameState tgs preGuses fb