{-| Author: Xiuge Chen <xiugec@student.unimelb.edu.au>

    - Purpose: A simplified Battleship game guesser that finds the location
      of hidden battleships using as little as possible number of guesses with
      the help of some additional feedback information.

      The game is played on a 4×8 grid with two users, one searcher trying 
      to find the locations of three battleships hidden by the other user, the
      hider.
    
      Additional information will be provided after each guess:
        1. number of ships exactly located
        2. number of guesses that were exactly one space away from a ship
        3. number of guesses that were exactly two spaces away from a ship

    - Assumptions: 

    - Running Flow: starting with calling the "initialGuess" and many 
      consecutive calls to "nextGuess" until all three battleships are 
      identified. For each "nextGuess", the "feedback" function will give 
      addition information described above, so that the next "nextGuess" could 
      use it to update "GameState" and determine its guess.
-}

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List

-- **************** Data Definition ****************

-- Possible row of the game
-- Note: Order does matter, GameState will use the same order of row 
-- and the number of '|' between two rows is used to represent distance
data Row = One | Two | Three | Four
    deriving (Eq, Ord, Enum)

instance Show Row where 
    show One   = "1"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"

-- Possible column of the game
-- Note: Order does matter, GameState will use the same order of col 
-- and the number of '|' between two cols is used to represent distance
data Col = A | B | C | D | E | F | G | H
    deriving (Eq, Ord, Show, Enum)

-- All different combination of location of the game
data Location = Location Col Row 
    deriving (Eq, Ord)

instance Show Location where 
    show (Location col row) = show col ++ show row

-- Feedback, a 3-tuple described above
type Feedback = (Int, Int, Int)

-- The state of game, including all possible targets, get updates after each feedback
type GameState = [[Location]]

-- **************** Constants ****************

-- number of target in this game
numTargets :: Int
numTargets = 3

-- 
firstGuess :: [Location]
firstGuess = [(Location A Four), (Location H Two), (Location H Four)]

-- All possible locations
allLoc :: [Location]
allLoc = [Location col row | col <- [A ..], row <- [One ..]]

-- Initial game state, turn each Location into a tuple (Location, True)
initGameState :: GameState
initGameState = subseqFixLen 3 allLoc 

-- **************** Functions ****************

-- all subseqences of specified fixed length
subseqFixLen :: Int -> [a] -> [[a]]
subseqFixLen 0 _  = [ [] ]
subseqFixLen k xs = [ y:zs | (y:ys) <- tails xs, zs <- subseqFixLen (k-1) ys]

-- Convert a string to Row
toRow :: String -> Row
toRow r
    | r == "1"  = One
    | r == "2"  = Two
    | r == "3"  = Three
    | r == "4"  = Four
    | otherwise = error "Undefined string representation of row"

-- Convert a string to Col
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

-- Convert a String to Location, Nothing returned if input invalid
toLocation :: String -> Maybe Location
toLocation s =
    if length s == 2
        then Just (Location (toCol(take 1 s)) (toRow(drop 1 s)))
    else Nothing

-- Calculate the distance between two location
locationDist :: Location -> Location -> Int
locationDist (Location c1 r1) (Location c2 r2)
    = max colDist rowDist
    where colDist = abs (fromEnum c1 - fromEnum c2)
          rowDist = abs (fromEnum r1 - fromEnum r2)

-- Get the minimum distance between one guess and all targets.
-- Assumed each target is different
minGusDist :: Location -> [Location] -> Int
minGusDist _ []         = 99 -- a suficient large number larger than max(# row, # col)
minGusDist gus (tg:tgs) = min (locationDist gus tg) (minGusDist gus tgs)

-- Take a list of targets and a guesses
-- Assumed each guess is different, and each target is different
-- Returns the appropriate 3-tuple feedback described in the header section.
-- Note: each guess will only be mapped to the closest target
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ []           = (0, 0, 0)
feedback ts (g:gs) 
    | minDist == 0 = (a+1, b, c)
    | minDist == 1 = (a, b+1, c)
    | minDist == 2 = (a, b, c+1)
    | otherwise    = (a, b, c)
    where minDist = minGusDist g ts
          (a,b,c) = feedback ts gs

-- Hard-coded optimal initial guess to help minimaize the guess number.
initialGuess :: ([Location], GameState)
initialGuess = (firstGuess, initGameState)

-- Update game state based on previous guess and feedback recieved
-- eliminate all grids that are not possible to contain targets
updateState :: [Location] -> GameState -> Feedback -> GameState
updateState gus gst fb = [tg | tg <- gst, feedback tg gus == fb && gus /= tg]

-- calculate the expected number of remaining targets if we make this guess
expectRemain :: [Location] -> GameState -> Double
expectRemain gus tgs = sum (map (\x -> x * x / totalNum) result)
    where result = map (fromIntegral.length) (group (sort (map (`feedback` gus) tgs)))
          totalNum = fromIntegral (length tgs)

-- choose the guess that has lowest expected number of remaining targets
chooseGuess :: GameState -> GameState -> ([Location], Double)
chooseGuess [] gst = ([], 9999)
chooseGuess (gus:guses) gst
    | currentScore < snd other = (gus, currentScore)
    | otherwise = other
    where currentScore = expectRemain gus gst
          other = chooseGuess guses gst

-- Make next guess based on previous guess, game state before that guess
-- and the feedback recieved
nextGuess :: ([Location], GameState) -> Feedback
              -> ([Location], GameState)
nextGuess (locs, gst) fb = (fst (chooseGuess newState newState), newState)
    where newState = updateState locs gst fb