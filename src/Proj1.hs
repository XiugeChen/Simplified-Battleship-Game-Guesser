{-| Author: Xiuge Chen <xiugec@student.unimelb.edu.au>

    - Purpose: A simplified Battleship game guesser that finds the location
      of hidden battleships using as little as possible number of guesses with
      the help of some additional feedback information.

      The game is played on a 4×8 grid with two users, one searcher trying 
      to find the locations of three battleships hidden by the other user, the
      hider.
    
      Additional information will be provided after each guess:
        1. the number of ships exactly located
        2. the number of guesses that were exactly one space away from a ship
        3. the number of guesses that were exactly two spaces away from a ship

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

instance Show Row where show = showRow

-- Possible column number of the map
data Col = A | B | C | D | E | F | G | H 
    deriving (Eq, Show)

-- The location of a battleship, defined by a grid on specific row and column
data Location = Location Col Row 
    deriving (Eq)

instance Show Location where show = showLocation

-- ex
-- data GameState = Null

-- **************** Functions ****************

-- Takes a row variable and convert it to a showable String
-- Implementation of Show type class
showRow :: Row -> String
showRow r
    | r == One   = "1"
    | r == Two   = "2"
    | r == Three = "3"
    | r == Four  = "4"
    | otherwise  = error "Undefined show behaviour for row"

-- Takes a location and convert it to a showable String
-- Implementation of Show type class
showLocation :: Location -> String
showLocation (Location c r) = show c ++ show r

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
    | otherwise = error "Undefined char representation of row"

-- Takes a String format location and convert it type Location
-- Input must be a valid location (i.e. length two from A1 to H4)
toLocation :: String -> Maybe Location
toLocation s =
    if length s == 2
        then Just (Location (toCol(take 1 s)) (toRow(drop 1 s)))
    else Nothing

{-
-- ex
-- feedback :: [Location] → [Location] → (Int,Int,Int)

-- ex
-- initialGuess :: ([Location],GameState)

-- ex
-- nextGuess :: ([Location],GameState) → (Int,Int,Int) → ([Location],GameState)
-}