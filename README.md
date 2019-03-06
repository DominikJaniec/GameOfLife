# GameOfLife

Conway's Game of Life in F#

## Rules

The universe of the _Game of Life_ is an infinite, two-dimensional orthogonal grid of square _cells_, each of which is in one of two possible states, **alive** or **dead**. Every _cell_ interacts with its eight neighbours, which are the _cells_ that are horizontally, vertically, or diagonally adjacent.

At each step in time, the following transitions occur:

1. Any **live** _cell_ with _fewer than two_ live neighbours **dies**, as if by underpopulation.
2. Any **live** _cell_ with _two or three_ live neighbours **lives** on to the next generation.
3. Any **live** _cell_ with _more than three_ live neighbours **dies**, as if by overpopulation.
4. Any **dead** _cell_ with _exactly three_ live neighbours becomes a **live** _cell_, as if by reproduction.

From _Wikipedia_: https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
