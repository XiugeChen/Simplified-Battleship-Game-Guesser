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

-- **************** Data Definition ****************

-- Possible row of the game
-- Note: Order does matter, GameState will use the same order of row 
-- and the number of '|' between two rows is used to represent distance
data Row = One | Two | Three | Four 
    deriving (Eq, Enum)

instance Show Row where 
    show One   = "1"
    show Two   = "2"
    show Three = "3"
    show Four  = "4"

-- Possible column of the game
-- Note: Order does matter, GameState will use the same order of col 
-- and the number of '|' between two cols is used to represent distance
data Col = A | B | C | D | E | F | G | H 
    deriving (Eq, Show, Enum)

-- All different combination of location of the game
data Location = Location Col Row 
    deriving (Eq)

instance Show Location where show = locToStr

-- Feedback, a 3-tuple described above
type Feedback = (Int, Int, Int)

-- The state of each grid in the form (Location, Bool), where Bool 
-- indicates whether possible to contain a target
type GridState = (Location, Bool)

-- The state of game (2D), gets updated everytime recieved some feedback
type GameState = [[GridState]]

-- **************** Constants ****************

numTargets :: Int
numTargets = 3

-- All possible location in 2 dimension (row * col)
-- The order of each row/col is specified by the order of data definition
allLoc :: [[Location]]
allLoc = [[Location col row | col <- [A ..]] | row <- [One ..]]

-- Initial game state, turn each Location into a tuple (Location, True)
initGameState :: GameState
initGameState = [[(x, True) | x <- row] | row <- allLoc]

-- **************** Functions ****************

-- Convert Location to a showable String
locToStr :: Location -> String
locToStr (Location col row) = show col ++ show row

-- Convert a string to Row
toRow :: String -> Row
toRow r
    | r == "1"  = One
    | r == "2"  = Two
    | r == "3"  = Three
    | r == "4"  = Four
    | otherwise = error "Undefined char representation of row"

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
    | otherwise = error "Undefined char representation of column"

-- Convert a String to Location, Nothing returned if input invalid
toLocation :: String -> Maybe Location
toLocation s =
    if length s == 2
        then Just (Location (toCol(take 1 s)) (toRow(drop 1 s)))
    else Nothing

-- Element wise add two triples of numbers
elemWiseAdd :: Num a => (a, a, a) -> (a, a, a) -> (a, a, a)
elemWiseAdd (a1, b1, c1) (a2, b2, c2) = (a1 + a2, b1 + b2, c1 + c2)

-- Calculate the distance of two Enum data
enumDist :: Enum a => a -> a -> Int
enumDist x y = abs (fromEnum x - fromEnum y)

-- Calculate the distance between two location
locationDist :: Location -> Location -> Int
locationDist (Location c1 r1) (Location c2 r2)
    = max (enumDist c1 c2) (enumDist r1 r2)

-- Get the minimum distance between one guess and all targets.
-- Assumed each target is different
minGusDist :: Location -> [Location] -> Int
-- Base case: a suficient large number larger than max(# row, # col)
minGusDist _ []         = 99 
minGusDist gus (tg:tgs) = min (locationDist gus tg) (minGusDist gus tgs)

-- Take a list of targets and a guesses
-- Assumed each guess is different, and each target is different
-- Returns the appropriate 3-tuple feedback described in the header section.
-- Note: each guess will only be mapped to the closest target
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback _ []           = (0, 0, 0)
feedback ts (g:gs) 
    | minDist == 0 = elemWiseAdd (1, 0, 0) rest
    | minDist == 1 = elemWiseAdd (0, 1, 0) rest
    | minDist == 2 = elemWiseAdd (0, 0, 1) rest
    | otherwise    = rest
    where minDist = minGusDist g ts
          rest    = feedback ts gs

-- Hard-coded optimal initial guess to help minimaize the guess number.
initialGuess :: ([Location], GameState)
initialGuess = ([(Location A One), (Location A Two), (Location A Three)]
               , initGameState)

-- Check whether a target is possible to be in one location
-- based on previous guess and feedback recieved
containTg :: Location -> [Location] -> Feedback -> Bool
containTg _ [] _                    = True
containTg grid locs (a, b, c)
    | minDist == 0 && a > 0       = True
    | minDist == 1 && b > 0       = True
    | minDist == 2 && c > 0       = True
    | minDist > 2  && 3-a-b-c > 0 = True
    | otherwise                   = False
    where minDist = minGusDist grid locs

-- Update grid state based on previous guess and feedback recieved
updateGrid :: GridState -> [Location] -> Feedback -> GridState
updateGrid grist [] _ = grist
updateGrid (grid, state) locs fb = (grid, state && existTg)
    where existTg = containTg grid locs fb

-- Update game state based on previous guess and feedback recieved
-- eliminate all grids that are not possible to contain targets
updateState :: [Location] -> GameState -> Feedback -> GameState
updateState [] gst _           = gst
updateState locs gst fb = [[updateGrid x locs fb | x <- row] | row <- gst]

-- Make guesses based on current game state
-- First arg is just a list to hold results
makeGuess :: [Location] -> GameState -> [Location]
makeGuess gus []       = gus
makeGuess gus ([]:gst) = makeGuess gus gst
makeGuess gus ((x:row):gst)
    | length gus == numTargets = gus
    | snd x                    = makeGuess ((fst x):gus) gst
    | otherwise                = makeGuess gus gst

-- Make next guess based on previous guess, game state before that guess
-- and the feedback recieved
nextGuess :: ([Location], GameState) -> Feedback
              -> ([Location], GameState)
nextGuess (locs, gst) fb = ((makeGuess [] newState), newState)
    where newState = updateState locs gst fb