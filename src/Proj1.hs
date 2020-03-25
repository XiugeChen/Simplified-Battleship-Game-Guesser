{-| Author:    Xiuge Chen <xiugec@student.unimelb.edu.au>

    Simplitied Battleship Guesser

    The game is played on a 4×8 grid, and involves two players:
        1. A hider who will hide 3 battleships on the map, where each battleship takes one grid.
        2. A searcher who will try to find the locations of these three battleships.
    
    Additional information that the hider will provide after each guess:
        1. the number of ships exactly located;
        2. the number of guesses that were exactly one space away from a ship; and
        3. the number of guesses that were exactly two spaces away from a ship.

    The program plays the role of searcher and tries to find the correct locations of all 
    three battleships with the minimum number of guessing.
-}

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where
