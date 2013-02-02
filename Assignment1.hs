{-
 - Starting code for Assignment 1 in CISC 260, Winter 2013.
 - This module, when finished, will find the most profitable path across a
 - chessboard with coins on the squares.  The path must start at the top of the
 - board and finish at the bottom, moving down one square either straight or diagonally
 - at each move.
 -
 - Author: Margaret Lamb
 - Add you name(s) or student id number(s) to this header comment.
 -}
module Assignment1 where

-- This type represents a puzzle board -- a two-dimensional list of integers, each representing
-- the number of coins on a square.  The board is stored in "row-major order".  The first sub-list
-- (element 0 of the board) is the first row of the board.
type Board = [[Int]]

-- the board shown on the web site
sampleBoard1 :: Board
sampleBoard1 = [[5,4,3,1],[10,2,1,0],[0,1,2,0],[2,3,4,20]]

-- a 3x3 board for testing
sampleBoard2 :: Board
sampleBoard2 = [
    [1, 1, 10],
    [2, 0, 1],
    [6, 4, 3]]
    
-- an 8x8 board for testing
sampleBoard3 :: Board
sampleBoard3 = [
    [16, 12,  0, 17,  6,  2, 11,  8],
    [13, 20, 18,  4,  4,  3,  3,  2],
    [ 5,  4,  4,  4,  4,  2,  3,  3],
    [ 6,  5,  1,  7,  4,  2, 20, 18],
    [ 5, 19, 12,  6, 19,  4, 14,  1],
    [18, 16,  1,  0, 19,  4,  3,  3],
    [16, 10,  3, 17, 11, 17, 10,  3],
    [ 3,  8,  0,  5,  18, 2,  7, 20]]
	
-- a 4x5 board, to make sure bestPath works with boards that aren't square
sampleBoard5 :: Board
sampleBoard5 = [[5, 4,3,8,6],
                [10,2,1,0,7],
                [0, 1,9,4,3],
                [2, 3,4,0,20]]


-- A path through the rows (or some of the rows) of a board.
-- First element is sequence of columns, second is total coins
type Path = ([Int],Int)
      
{-
 - Finds the best path from the top to the bottom of a board with coins on it.
 - The rules of the puzzle are on the web site.  This function assumes the board is 
 - legal -- i.e. all rows are the same width and there are no negative numbers
 - of coins.  
 -}
bestPath :: Board -> Path
-- The "stub" below is there so that this module will load without error as is.  
-- Replace it with a real implementation.
-- If there is a tie for the best path, it doesn't matter which of the best
-- paths your function returns
bestPath board = ([],0) 

		
-- ADD YOUR HELPER FUNCTIONS HERE.
-- Your helper functions must include the two described in the web site, but you can
-- add more of your own if you wish.
maxPath :: [Path] -> Path
maxPath [] 			= error "maximum of empty path"
maxPath [x] 			= x
maxPath (x:xs)
	| snd x > snd maxTail 	= x
	| otherwise 		= maxTail
	where maxTail 		= maxPath xs

bestPathStarting :: Board -> Int -> Path
bestPathStarting [] _ 		= ([],0)
bestPathStarting [x] i 		= ([i], x !! i)
bestPathStarting (x:xs) i	= maxPath [(x, (x !! i)), (bestPathStarting xs (i+1)), (bestPathStarting xs (i-1))]

path1 :: Path
path1 = ([1,2],3)

path2 :: Path
path2 = ([2,1],5)


path3 :: Path
path3 = ([4,5,6],8)
        
