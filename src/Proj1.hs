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

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where

-- **************** Data ****************

-- ex
data Location = Empty

-- ex
data GameState = Null

-- **************** Functions ****************

-- ex
toLocation :: String → Maybe Location

-- ex
feedback :: [Location] → [Location] → (Int,Int,Int)

-- ex
initialGuess :: ([Location],GameState)

-- ex
nextGuess :: ([Location],GameState) → (Int,Int,Int) → ([Location],GameState)