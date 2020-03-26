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

module Proj1 (Location, toLocation{-, feedback,
              GameState, initialGuess, nextGuess-}) where

-- **************** Main Data ****************

-- Possible row number of the map
data Row = One | Two | Three | Four 
    deriving (Eq)

instance Show Row where show = rowToStr

-- Possible column number of the map
data Col = A | B | C | D | E | F | G | H 
    deriving (Eq, Show)

-- The location of a battleship, defined by a grid on specific row and column
data Location = Location Col Row 
    deriving (Eq)

instance Show Location where show = locToStr

-- ex
-- data GameState = Null

-- **************** Functions ****************

-- Takes a row variable and convert it to a showable String
-- Implementation of Show type class
rowToStr :: Row -> String
rowToStr r
    | r == One   = "1"
    | r == Two   = "2"
    | r == Three = "3"
    | r == Four  = "4"
    | otherwise  = error "Undefined instance of row"

-- Convert Row value to int for calculating distance
rowToInt :: Row -> Int
rowToInt r
    | r == One   = 1
    | r == Two   = 2
    | r == Three = 3
    | r == Four  = 4
    | otherwise  = error "Undefined instance of row"

-- Convert Col value to int for calculating distance
colToInt :: Col -> Int
colToInt c
    | c == A = 1
    | c == B = 2
    | c == C = 3
    | c == D = 4
    | c == E = 5
    | c == F = 6
    | c == G = 7
    | c == H = 8
    | otherwise = error "Undefined instance of column"

-- Takes a location and convert it to a showable String
-- Implementation of Show type class
locToStr :: Location -> String
locToStr (Location col row) = show col ++ show row

-- Convert a string to Row
-- Input string must be of valid row format (length 1 from "1" to "4")
toRow :: String -> Row
toRow r
    | r == "1"  = One
    | r == "2"  = Two
    | r == "3"  = Three
    | r == "4"  = Four
    | otherwise = error "Undefined char representation of row"

-- Convert a string to Col
-- Input string must be of valid column format (length 1 from "A" to "H")
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
    | otherwise = error "Undefined char representation of column"

-- Takes a String format location and convert it type Location
-- Input must be a valid location (i.e. length two from A1 to H4)
toLocation :: String -> Maybe Location
toLocation s =
    if length s == 2
        then Just (Location (toCol(take 1 s)) (toRow(drop 1 s)))
    else Nothing

-- Element wise add two triple of numbers
elemWiseAdd :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
elemWiseAdd (a1, b1, c1) (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

-- Calculate the difference of two columns
colDiff :: Col -> Col -> Int
colDiff c1 c2 = abs (colToInt c1 - colToInt c2)

-- Calculate the difference of two rows
rowDiff :: Row -> Row -> Int
rowDiff r1 r2 = abs (rowToInt r1 - rowToInt r2)

-- Get the minimum distance between one guess and all targets.
minGusDist :: Location -> [Location] -> Int
-- Base case: a suficient large number larger than max(# row, # col)
minGusDist _ []         = 99 
minGusDist (Location gc gr) ((Location tc tr):tgs) = 
    min (max (colDiff gc tc) (rowDiff gr tr))
        (minGusDist (Location gc gr) tgs)

-- Take a list of targets and a guesses
-- Returns the appropriate 3-tuple feedback described in the header section.
-- Note: each guess will only be mapped to the closest target
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ []           = (0, 0, 0)
feedback tgs (gus:guses) 
    | minDist == 0 = elemWiseAdd (1, 0, 0) rest
    | minDist == 1 = elemWiseAdd (0, 1, 0) rest
    | minDist == 2 = elemWiseAdd (0, 0, 1) rest
    | otherwise    = rest
    where minDist = minGusDist gus tgs
          rest    = feedback tgs guses

{-
-- ex
-- initialGuess :: ([Location],GameState)

-- ex
-- nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
-}