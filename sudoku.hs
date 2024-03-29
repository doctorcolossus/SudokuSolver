-- CS3210 - Principles of Programming Languages - Fall 2019
-- Programming Assignment 02 - A Sudoku Solver
-- Author(s): Casey Jones, Conor McCandless
-- Date: November 3rd, 2019

import System.Environment
import System.IO
import System.Exit
import Data.List
import Data.Maybe

type Sequence = [Int]
type Board    = [Sequence]

-- ***** HELPER FUNCTIONS *****

toInt :: [Char] -> Int
{- converts given parameter to integer
   input:   a string
   output:  the string converted to integer
   example: toInt "123" returns 123 -}
toInt s = read s :: Int

toIntList :: [Char] -> Sequence
{- converts given parameter to a sequence of integers (one digit at a time)
   input:   a string
   output:  the string converted into a sequence of integers
   example: toIntList "123" returns [1, 2, 3] -}
toIntList s = [ toInt [c] | c <- s ]

-- ***** GETTER FUNCTIONS *****

getBoard :: [Char] -> Board
{- convert given string to a sudoku board
   input:   a string (the board as read from a sudoku input file)
   output:  a sudoku board
   example: getBoard "530\n600\n098\n800\n" yields...
     [ [5,3,0],
       [6,0,0],
       [0,9,8] ] -}
getBoard board = [ toIntList line | line <- lines board ]

getNRows :: Board -> Int
{- given a board, return its number of rows
   input:   a board
   output:  number of rows
   example: getNRows
   [ [5,3,0,0,7,0,0,0,0],
     [6,0,0,1,9,5,0,0,0],
     [0,9,8,0,0,0,0,6,0],
     [8,0,0,0,6,0,0,0,3],
     [4,0,0,8,0,3,0,0,1],
     [7,0,0,0,2,0,0,0,6],
     [0,6,0,0,0,0,2,8,0],
     [0,0,0,4,1,9,0,0,5],
     [0,0,0,0,8,0,0,7,9] ] yields 9
   hint: use length
-}
getNRows board
  | null board = 0
  | otherwise  = length board

getNCols :: Board -> Int
{- given a board, return its number of columns or 0 if rows do not have the same number of columns
   input:  a board
   output: number of columns
   examples:
     getNCols [ [5,3,0,0,7,0,0,0,0],
                [6,0,0,1,9,5,0,0,0],
                [0,9,8,0,0,0,0,6,0],
                [8,0,0,0,6,0,0,0,3],
                [4,0,0,8,0,3,0,0,1],
                [7,0,0,0,2,0,0,0,6],
                [0,6,0,0,0,0,2,8,0],
                [0,0,0,4,1,9,0,0,5],
                [0,0,0,0,8,0,0,7,9] ] yields 9
     getNCols [ [5,3,0,0,7,0,0,0,0],
                [6,0,0,1,9,5,0,0,0],
                [0,9,8,0,0,0,6,0],
                [8,0,0,0,6,0,3],
                [4,0,0,8,0,3,0,0,1],
                [7,0,0,0,2,0,0,0,6],
                [0,6,0,0],
                [0,0,0,4,1,9,0,0,5],
                [0,0,0,0,8,0,0,7,9] ] yields 0
   hint: use length to create a list
         with all the sizes of each row from the board;
         then decide whether all of the rows have the same size,
         returning that size if yes, or 0 otherwise -}
getNCols board
  | null board = 0
  | not (all (\b -> length b == length (head board)) (tail board)) = 0
  | otherwise =
    length (head board)

getBox :: Board -> Int -> Int -> Sequence
{- given a board and box coordinates, return the corresponding box as a sequence
   input: a board and two integers (the box coordinates)
   output: a sequence
   example:
     getBox [ [5,3,0,0,7,0,0,0,0],
              [6,0,0,1,9,5,0,0,0],
              [0,9,8,0,0,0,0,6,0],
              [8,0,0,0,6,0,0,0,3],
              [4,0,0,8,0,3,0,0,1],
              [7,0,0,0,2,0,0,0,6],
              [0,6,0,0,0,0,2,8,0],
              [0,0,0,4,1,9,0,0,5],
              [0,0,0,0,8,0,0,7,9] ] 1 1 yields [0,8,0,6,0,2,0,3,0]
   hint: use list comprehension to filter the rows of the target box;
         then transpose what you got and apply the same reasoning to filter the columns;
         use concat to return the sequence -}
getBox board x y = concat
                   (transpose [take 3 (drop (3*x) b) |
                               b <- take 3 (drop (3*y) board)])

getEmptySpot :: Board -> (Int, Int)
{- given a board, return the first location
    that is empty (i.e., it has zero), if one exists;
    OK to assume that you will only call this function
   when you know that there is an empty spot
   input:   a board
   output:  a tuple with the coordinates (i, j) of the empty spot found
   example:
     getEmptySpot [ [5,3,0,0,7,0,0,0,0],
                    [6,0,0,1,9,5,0,0,0],
                    [0,9,8,0,0,0,0,6,0],
                    [8,0,0,0,6,0,0,0,3],
                    [4,0,0,8,0,3,0,0,1],
                    [7,0,0,0,2,0,0,0,6],
                    [0,6,0,0,0,0,2,8,0],
                    [0,0,0,4,1,9,0,0,5],
                    [0,0,0,0,8,0,0,7,9] ] yields (0,2) -}
getEmptySpot board = do
  let y = fromJust(findIndex (elem 0) board)
  let x = fromJust(findIndex (==0) (board !! y))
  (x, y)

getOptions :: Board -> Int -> Int -> [Int]
-- given a board and x & y coordinates, return its set of valid possible solutions
getOptions board x y = do
  let usedInBox = [b | b <- getBox board (div x 3) (div y 3), b /= 0]
  let usedInRow = [r | r <- board !! y, r /= 0]
  let usedInCol = [c | c <- [c !! x | c <- board], c /= 0]
  [o | o <- [1..9],
       not (elem o usedInBox),
       not (elem o usedInRow),
       not (elem o usedInCol)]

-- ***** PREDICATE FUNCTIONS *****

isGridValid :: Board -> Bool
{- given a board, return True/False
   depending whether the given board constitutes a valid grid
   (i.e., #rows = #cols = 9) or not
   input:  a board
   output: True/False
   examples:
     isGridValid [ [5,3,0,0,7,0,0,0,0],
                   [6,0,0,1,9,5,0,0,0],
                   [0,9,8,0,0,0,0,6,0],
                   [8,0,0,0,6,0,0,0,3],
                   [4,0,0,8,0,3,0,0,1],
                   [7,0,0,0,2,0,0,0,6],
                   [0,6,0,0,0,0,2,8,0],
                   [0,0,0,4,1,9,0,0,5],
                   [0,0,0,0,8,0,0,7,9] ] yields True
     isGridValid [ [5,3,0,0,7,0,0,0,0],
                   [6,0,0,1,9,5,0,0,0],
                   [8,0,0,0,6,0,0,0,3],
                   [4,0,0,8,0,3,0,0,1],
                   [7,0,0,0,2,0,0,0,6],
                   [0,6,0,0,0,0,2,8,0],
                   [0,0,0,4,1,9,0,0,5],
                   [0,0,0,0,8,0,0,7,9] ] returns False
     isGridValid [ [5,3,0,7,0,0,0,0],
                   [6,0,1,9,5,0,0,0],
                   [8,0,0,6,0,0,0,3],
                   [4,0,8,0,3,0,0,1],
                   [7,0,0,2,0,0,0,6],
                   [0,0,0,0,0,2,8,0],
                   [0,0,4,1,9,0,0,5],
                   [0,0,0,8,0,0,7,9] ] returns False
   hint: use getNRows and getNCols -}
isGridValid board = (getNRows board) == 9 && (getNCols board) == 9

isSequenceValid :: Sequence -> Bool
{- return True/False depending whether the given sequence is valid or not,
   according to sudoku rules
   input:  a sequence of digits from 0-9
   output: True/False
   examples:
     isSequenceValid [5,3,0,0,7,0,0,0,0] yields True
     isSequenceValid [5,3,0,5,7,0,0,0,0] yields False
   hint: build a list with the digits from the given sequence
         that are different than zero;
         then determine whether there are digits
         that repeat in the created list -}
isSequenceValid sequence = do
  let nonzero = filter (/=0) sequence
  nonzero == nub nonzero

areRowsValid :: Board -> Bool
{- return True/False depending whether ALL of the row sequences are valid or not
   input:  a board
   output: True/False
   hint:   use list comprehension and isSequenceValid -}
areRowsValid board =
   all (isSequenceValid) board

areColsValid :: Board -> Bool
{- return True/False depending whether ALL of the col sequences are valid or not
   input:  a board
   output: True/False -}
areColsValid board =
  areRowsValid (transpose board)

areBoxesValid :: Board -> Bool
{- return True/False depending whether ALL of the box sequences are valid or not
   input:  a board
   output: True/False
   hint:   use list comprehension, isSequenceValid, and getBox -}
areBoxesValid board =
  all isSequenceValid [getBox board x y |
                       (x, y) <- [(x, y) |
                                  x <- [0..2],
                                  y <- [0..2]]]

isValid :: Board -> Bool
{- return True/False
     depending whether the given board is valid sudoku configuration or not
   input:  a board
   output: True/False
   hint:   use isGridValid, areRowsValid, areColsValid, and areBoxesValid -}
isValid board = (isGridValid board) &&
                (areRowsValid board) &&
                (areColsValid board) &&
                (areBoxesValid board)

isCompleted :: Board -> Bool
{- return True/False depending whether the given board is completed or not;
     a board is considered completed if there isn't a single empty cell
   input:  a board
   output: True/False
   hint:   use list comprehension and the elem function -}
isCompleted board = and [not (elem 0 l) | l <- board]

isSolved :: Board -> Bool
{- return True/False depending whether the given board is solved or not;
     a board is solved if it is completed and still valid
   input:  a board
   output: True/False -}
isSolved board = (isCompleted board) && (isSolved board)

-- ***** SETTER FUNCTIONS *****

setRowAt :: Sequence -> Int -> Int -> Sequence
{- given a sequence, an index, and a value,
     writes the value at the index location, returning a new sequence,
     but only if the original value at the specified location is empty;
     otherwise, return the original sequence unchanged
   input:  a sequence, an index, and a value
   output: a new sequence
   examples:
     setRowAt [1, 2, 3, 0, 4, 5] 3 9 yields [1,2,3,9,4,5]
     setRowAt [1, 2, 3, 8, 4, 5] 3 9 yields [1,2,3,8,4,5]
   hint: use concatenation, take, and drop -}
setRowAt sequence index value
  | sequence !! index == 0 =
    (take index sequence) ++ [value] ++ (drop (index+1) sequence)
  | otherwise = sequence

setBoardAt :: Board -> Int -> Int -> Int -> Board
{- given a board, two indexes i and j (representing coordinates), and a value,
     writes the value at the (i, j) coordinate, returning the new board,
     but only if the original value at the specified location is empty;
     otherwise, return the original board unchanged
   input:  a board, two indexes (i, j), and a value
   output: a new board
   examples:
     setBoardAt [ [5,3,0,0,7,0,0,0,0],
                  [6,0,0,1,9,5,0,0,0],
                  [0,9,8,0,0,0,0,6,0],
                  [8,0,0,0,6,0,0,0,3],
                  [4,0,0,8,0,3,0,0,1],
                  [7,0,0,0,2,0,0,0,6],
                  [0,6,0,0,0,0,2,8,0],
                  [0,0,0,4,1,9,0,0,5],
                  [0,0,0,0,8,0,0,7,9] ] 0 2 4 yields
     setBoardAt [ [5,3,4,0,7,0,0,0,0],
                  [6,0,0,1,9,5,0,0,0],
                  [0,9,8,0,0,0,0,6,0],
                  [8,0,0,0,6,0,0,0,3],
                  [4,0,0,8,0,3,0,0,1],
                  [7,0,0,0,2,0,0,0,6],
                  [0,6,0,0,0,0,2,8,0],
                  [0,0,0,4,1,9,0,0,5],
                  [0,0,0,0,8,0,0,7,9] ]
   hint: use concatenation and setRowAt -}
setBoardAt board x y value
  | ((board !! y) !! x) == 0 =
    (take y board) ++ [setRowAt (board !! y) x value] ++ (drop (y+1) board)
  | otherwise = board

buildChoices :: Board -> Int -> Int -> [Board]
{- generate ALL possible boards, replacing the cell at (i, j)
     with ALL possible digits from 1 to 9;
     OK to assume that the cell at (i, j) is empty
   input:  a board and two indexes (i, j)
   output: a list of boards from the original board
   example:
     buildChoices [ [5,3,0,0,7,0,0,0,0],
                    [6,0,0,1,9,5,0,0,0],
                    [0,9,8,0,0,0,0,6,0],
                    [8,0,0,0,6,0,0,0,3],
                    [4,0,0,8,0,3,0,0,1],
                    [7,0,0,0,2,0,0,0,6],
                    [0,6,0,0,0,0,2,8,0],
                    [0,0,0,4,1,9,0,0,5],
                    [0,0,0,0,8,0,0,7,9] ] 0 2
          yields: [ [ [5,3,1,0,7,0,0,0,0],
                      [6,0,0,1,9,5,0,0,0],
                      [0,9,8,0,0,0,0,6,0],
                      [8,0,0,0,6,0,0,0,3],
                      [4,0,0,8,0,3,0,0,1],
                      [7,0,0,0,2,0,0,0,6],
                      [0,6,0,0,0,0,2,8,0],
                      [0,0,0,4,1,9,0,0,5],
                      [0,0,0,0,8,0,0,7,9] ],
                    [ [5,3,2,0,7,0,0,0,0],
                      [6,0,0,1,9,5,0,0,0],
                      [0,9,8,0,0,0,0,6,0],
                      [8,0,0,0,6,0,0,0,3],
                      [4,0,0,8,0,3,0,0,1],
                      [7,0,0,0,2,0,0,0,6],
                      [0,6,0,0,0,0,2,8,0],
                      [0,0,0,4,1,9,0,0,5],
                      [0,0,0,0,8,0,0,7,9] ],
                    ...
                    [ [5,3,9,0,7,0,0,0,0],
                      [6,0,0,1,9,5,0,0,0],
                      [0,9,8,0,0,0,0,6,0],
                      [8,0,0,0,6,0,0,0,3],
                      [4,0,0,8,0,3,0,0,1],
                      [7,0,0,0,2,0,0,0,6],
                      [0,6,0,0,0,0,2,8,0],
                      [0,0,0,4,1,9,0,0,5],
                      [0,0,0,0,8,0,0,7,9] ]
                    ] -}
buildChoices board x y =
  [setBoardAt board x y value | value <- (getOptions board x y)]

solve :: Board -> [Board]
{- given a board, finds all possible solutions
   (dead ends and invalid intermediate solutions are listed as empty boards)
   input:       a board
   output:      a list of boards from the original board -}
solve board
  | isCompleted board = 
    if isValid board
     then [board]
     else [[[]]]
  | otherwise = concat [solve choice |
                        choice <- buildChoices board i j ]
                where
                  emptySpot = getEmptySpot board
                  i = fst emptySpot
                  j = snd emptySpot

printBoard :: Board -> IO ()
-- pretty-print a given board
printBoard board = mapM_ putStrLn
                   [id (intercalate " " row) |
                    row <- [[show row |
                             row <- b] |
                            b <- board]
                    ++ [[""]]]

main = do -- program starts here

  args <- getArgs

  if (length args /= 1) then do -- validate the command-line
    putStrLn "Error: Expected exactly one argument: path to board file!"
    putStrLn "Aborting..."
    exitFailure
    else return()

  b <- readFile (head args) -- read in the specified board file

  let board = getBoard b -- create a board from the string board
  
  putStrLn "problem:"
  printBoard board

  let solutions = [s | s <- solve board, s /= [[]]] -- ignore empty results

  putStrLn "solutions:"
  mapM_ printBoard solutions

  print "Done!"

-- vim: et ts=2 sts=2 sw=2
