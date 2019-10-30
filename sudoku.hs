-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Casey Jones, Conor McCandless
-- Date: November 3rd, 2019

import System.Environment
import System.IO
import Data.List

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

-- name: toInt
-- description: converts given parameter to integer
-- input: a string
-- output: the string converted to integer
-- example: toInt "123" returns 123
toInt :: [Char] -> Int
toInt s = read s :: Int

-- name: toIntList
-- description: converts given parameter to a sequence of integers (one digit at a time)
-- input: a string
-- output: the string converted into a sequence of integers
-- example: toIntList "123" returns [1, 2, 3]
toIntList :: [Char] -> Sequence
toIntList s = [ toInt [c] | c <- s ]

-- ***** GETTER FUNCTIONS *****

-- TODO #1
-- name: getBoard
-- description: convert given string to a sudoku board
-- input: a string (the board as read from a sudoku input file)
-- output: a sudoku board
-- example: getBoard "530070000\n600195000\n098000060\n800060003\n400803001\n700020006\n060000280\n000419005\n000080079\n" yields
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- hint: use lines to convert the string into a list of strings, and then apply toIntList on each of the strings of the list to return the board
-- getBoard :: [Char] -> Board

-- TODO #2
-- name: getNRows
-- description: given a board, return its number of rows
-- input: a board
-- output: number of rows
-- example: getNRows
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields 9
-- hint: use length
-- getNRows :: Board -> Int

-- TODO #3
-- name: getNCols
-- description: given a board, return its number of columns or 0 if rows do not have the same number of columns
-- input: a board
-- output: number of columns
-- example 1: getNCols
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields 9
-- example 2: getNCols
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,6,0],
--   [8,0,0,0,6,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields 0
-- hint: use length to create a list with all the sizes of each row from the board; then decide whether all of the rows have the same size, returning that size if yes, or 0 otherwise
-- getNCols :: Board -> Int

-- TODO #4
-- name: getBox
-- description: given a board and box coordinates, return the correspondent box as a sequence
-- input: a board and two integer (box coordinates)
-- output: a sequence
-- example: getBox
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 1 1 yields [0,8,0,6,0,2,0,3,0]
-- hint: use list comprehension to filter the rows of the target box; then transpose what you got and apply the same reasoning to filter the columns; use concat to return the sequence
-- getBox :: Board -> Int -> Int -> Sequence

-- TODO #5
-- name: getEmptySpot
-- description: given a board, return the first location that is empty (i.e., it has zero), if one exists; OK to assume that you will only call this function when you know that there is an empty spot
-- input: a board
-- output: a tuple with the coordinates (i, j) of the empty spot found
-- example: getEmptySpot
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields (0,2)
-- hint: use list comprehension to generate all the coordinates of the board that are empty; use head to return the first coordinate of your list
-- getEmptySpot :: Board -> (Int, Int)

-- ***** PREDICATE FUNCTIONS *****

-- TODO #6
-- name: isGridValid
-- description: given a board, return True/False depending whether the given board constitutes a valid grid (i.e., #rows = #cols = 9) or not
-- input: a board
-- output: True/False
-- example 1: isGridValid
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] yields True
-- example 2:
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] returns False
-- example 3:
-- [ [5,3,0,7,0,0,0,0],
--   [6,0,1,9,5,0,0,0],
--   [8,0,0,6,0,0,0,3],
--   [4,0,8,0,3,0,0,1],
--   [7,0,0,2,0,0,0,6],
--   [0,0,0,0,0,2,8,0],
--   [0,0,4,1,9,0,0,5],
--   [0,0,0,8,0,0,7,9] ] returns False
-- hint: use getNRows and getNCols
-- isGridValid :: Board -> Bool

-- TODO #7
-- name: isSequenceValid
-- description: return True/False depending whether the given sequence is valid or not, according to sudoku rules
-- input: a sequence of digits from 0-9
-- output: True/False
-- example 1: isSequenceValid [5,3,0,0,7,0,0,0,0] yields True
-- example 2: isSequenceValid [5,3,0,5,7,0,0,0,0] yields False
-- hint: build a list with the digits from the given sequence that are different than zero; then determine whether there are digits that repeats in the created list
-- isSequenceValid :: Sequence -> Bool

-- TODO #8
-- name: areRowsValid
-- description: return True/False depending whether ALL of the row sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension and isSequenceValid
-- areRowsValid :: Board -> Bool

-- TODO #9
-- name: areColsValid
-- description: return True/False depending whether ALL of the col sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use areRowsValid of the transposed board
-- areColsValid :: Board -> Bool

-- TODO #10
-- name: areBoxesValid
-- description: return True/False depending whether ALL of the box sequences are valid or not
-- input: a board
-- output: True/False
-- hint: use list comprehension, isSequenceValid, and getBox
-- areBoxesValid :: Board -> Bool

-- TODO #11
-- name: isValid
-- description: return True/False depending whether the given board is valid sudoku configuration or not
-- input: a board
-- output: True/False
-- hint: use isGridValid, areRowsValid, areColsValid, and areBoxesValid
-- isValid :: Board -> Bool

-- TODO #12
-- name: isCompleted
-- description: return True/False depending whether the given board is completed or not; a board is considered completed if there isn't a single empty cell
-- input: a board
-- output: True/False
-- hint: use list comprehension and the elem function
-- isCompleted :: Board -> Bool

-- TODO #13
-- name: isSolved
-- description: return True/False depending whether the given board is solved or not; a board is solved if it is completed and still valid
-- input: a board
-- output: True/False
-- isSolved :: Board -> Bool

-- ***** SETTER FUNCTIONS *****

-- TODO #14
-- name: setRowAt
-- description: given a sequence, an index, and a value, writes the value at the index location, returning a new sequence, but only if the original value at the specified location is empty; otherwise, return the original sequence unchanged
-- input: a sequence, an index, and a value
-- output: a new sequence
-- example 1: setRowAt [1, 2, 3, 0, 4, 5] 3 9 yields [1,2,3,9,4,5]
-- example 2: setRowAt [1, 2, 3, 8, 4, 5] 3 9 yields [1,2,3,8,4,5]
-- hint: use concatenation, take, and drop
-- setRowAt :: Sequence -> Int -> Int -> Sequence

-- TODO #15
-- name: setBoardAt
-- description: given a board, two indexes i and j (representing coordinates), and a value, writes the value at the (i, j) coordinate, returning the new board, but only if the original value at the specified location is empty; otherwise, return the original board unchanged
-- input: a board, two indexes (i, j), and a value
-- output: a new board
-- example 1: setBoardAt
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 4 yields
-- [ [5,3,4,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- hint: use concatenation and setRowAt
-- setBoardAt :: Board -> Int -> Int -> Int -> Board

-- TODO #16
-- name: buildChoices
-- description: given a board and a two indexes i and j (representing coordinates), generate ALL possible boards, replacing the cell at (i, j) with ALL possible digits from 1 to 9; OK to assume that the cell at (i, j) is empty
-- input: a board and two indexes (i, j)
-- output: a list of boards from the original board
-- example: buildChoices
-- [ [5,3,0,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ] 0 2 yields
-- [
-- [ [5,3,1,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- [ [5,3,2,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ],
-- ...
-- [ [5,3,9,0,7,0,0,0,0],
--   [6,0,0,1,9,5,0,0,0],
--   [0,9,8,0,0,0,0,6,0],
--   [8,0,0,0,6,0,0,0,3],
--   [4,0,0,8,0,3,0,0,1],
--   [7,0,0,0,2,0,0,0,6],
--   [0,6,0,0,0,0,2,8,0],
--   [0,0,0,4,1,9,0,0,5],
--   [0,0,0,0,8,0,0,7,9] ]
-- ]
-- hint: use list comprehension and the function setBoardAt
-- buildChoices :: Board -> Int -> Int -> [Board]

-- name: solve
-- description: given a board, finds all possible solutions (note that dead ends or invalid intermediate solutions are listed as empty boards)
-- input: a board
-- output: a list of boards from the original board
-- note: this code is given to you (just uncomment it when you are ready to test the solver)
-- solve :: Board -> [Board]
-- solve board
--   | isSolved board = [board]
--   | isCompleted board = [[[]]]
--   | not (isValid board) = [[[]]]
--   | otherwise = concat [ solve choice | choice <- buildChoices board i j ]
--     where
--       emptySpot = getEmptySpot board
--       i = fst emptySpot
--       j = snd emptySpot

-- program starts here
main = do

  -- validate the command-line and get the file name containing the board
  args <- getArgs
  if (length args /= 1)
    then do
      putStrLn "Error: Expected exactly one argument, containing the path of the board file!"
      putStrLn "Aborting..."
      exitFailure
    else -- TODO #18: read the contents of the board file into a string
      return()

  -- TODO #19: create a board from the string board (hint: use getBoard)

  -- TODO #20: use solve to find the solutions, disconsidering the ones that are [[]]

  -- TODO #21: print the solutions found

  print "Done!"
