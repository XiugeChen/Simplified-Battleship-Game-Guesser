# Simplified Battleship Game Guesser

## Introduction

* Project 1 for [COMP90048](https://handbook.unimelb.edu.au/2020/subjects/comp90048) (Declarative Programming) at the University of Melbourne, 2020 Sem1.

* The game is played on a 4×8 grid, and involves two players:

    1. A hider who will hide 3 battleships on the map, where each battleship takes one grid.

    2. A searcher who will try to find the locations of these three battleships.
    
* Additional information that the hider will provide after each guess:

    1. the number of ships exactly located;

    2. the number of guesses that were exactly one space away from a ship; and

    3. the number of guesses that were exactly two spaces away from a ship.

* The program plays the role of searcher and tries to find the correct locations of all three battleships with the minimum number of guessing.

* The guessing process should be started by calling the "initialGuess" and many consecutive calls to "nextGuess" until all three battleships are identified. For each "nextGuess", the "feedback" function will give addition information described above, so that the next "nextGuess" could use it to update "GameState" and determine the next guess.

* Result: 

* For detailed description please check out [project specification](docs/specification.pdf)

## Contribution
Xiuge Chen

xiugec@student.unimelb.edu.au

Subject: COMP90048 Declarative Programming

University of Melbourne