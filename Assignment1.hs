{-
 - Assignment 1 CISC 260, Winter 2013.
 - This module finds the most profitable path across a
 - chessboard with coins on the squares.  The path must start at the top of the
 - board and finish at the bottom, moving down one square either straight or diagonally
 - at each move.
 -
 - Author: Margaret Lamb, Cliff Coulter 04192150
 -}
module Assignment1 where

-- This type represents a puzzle board -- a two-dimensional list of integers, each representing
-- the number of coins on a square.  The board is stored in "row-major order".  The first sub-list
-- (element 0 of the board) is the first row of the board.
type Board = [[Int]]

-- small board for testing
sampleBoard0 :: Board
sampleBoard0 = [[1,2,3],[3,2,5]]

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
bestPath []	= ([],0)
bestPath [[]]	= ([],0)
bestPath x 	= maxPath [bestPathStarting x i|i<-[0..length x-1]]
	
{-
 - Finds the maximum path given a set of paths as a function of the value of the    2nd element.
-}
maxPath :: [Path] -> Path
maxPath [] 			= ([],0)
maxPath [x] 			= x
maxPath (x:xs)		
	| snd x > snd maxTail	= x
	| otherwise 		= maxTail
	where maxTail 		= maxPath xs

{-
 - Finds the best path given a board and a starting column value.
 - i,j represent indices
-}
bestPathStarting :: Board -> Int -> Path
bestPathStarting [] _ 		= ([],0)
bestPathStarting [x] i		= ([i], x !! i)
bestPathStarting (x:xs) i		
	| i > length x		= error "Index larger than column length."
	| otherwise		= (i:y, (x !! i) + j)
	where (y,j)		= maxPath [bestPathStarting xs k|k<-[i-1,i,i+1], k >= 0, k < length x]
