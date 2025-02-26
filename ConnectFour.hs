-- {-# OPTIONS -Wall #-}


module ConnectFour where

import Data.List

-- the game board is represented as a list of Int types.
-- The value of the cell indexed in Game means: Max player is repesented as 1 and min player is 0. -1 represents an unplayed cell.
type Game = [Int]

--the Role is represented as an Int type:
type Role = Int
-- the Cell type represents any one of the points on the board, given by its (row, column) pair.
type Cell = (Int,Int)


gridrow_game::Int
gridrow_game = 4
gridcol_game :: Int
gridcol_game = 4
win_length :: Int
win_length = 4

emptyCell :: Int
emptyCell = -1

humanPlayer :: Role
humanPlayer = 1      --- the value of the human player's step in a cell indexed in Game is 1.

compPlayer :: Role
compPlayer = 0    --- the value of the human player's step in a cell indexed in Game is 0.

searchSpace :: Int
searchSpace =12

-- Intialises the game board; -1 represents an emptyCell (unoccupied cell).
initGame::Game
initGame = take (gridcol_game * gridrow_game) (cycle [emptyCell])

-- maxPlayer function checks if the given player is max, and returns a Boolean.
maxPlayer::Role
maxPlayer = humanPlayer

-- minPlayer function checks if the given player is min, and returns a Boolean.
minPlayer :: Role
minPlayer = compPlayer



-- switch function alternates between players.
switch::Role->Role
switch p
    | (p == humanPlayer) = compPlayer
    | otherwise = humanPlayer



-- terminal function checks if the input game state is in a terminal state:
-- either a win state for a player or a state with no sucessors.
terminal::Game->Bool
terminal g = let occupiedCellsHuman = elemIndices humanPlayer g --- using predicate elemIndices to get the list of indexes of the celles occupied by the player.
                 occupiedCellsCompu = elemIndices compPlayer g
                 lines = getLines
    in if ([] /= (winLine lines occupiedCellsHuman) || [] /= (winLine lines occupiedCellsCompu)) then True
        else if not(elem (emptyCell) g)
          then True
          else False
        where winLine lines occuCells = [ l | l <- lines, (intersect l occuCells) == l]


--- for connectFour only the bottom cell is available
isbottomCell:: Game -> Int ->Bool
isbottomCell g cellId =
  if (cellId < (gridcol_game * (gridrow_game - 1))) && ((g !! (cellId + gridcol_game)) == emptyCell) --- the sell is on the bottom of the grid.
    then False
    else True

-- isMoveValid checks if a move made in a given game state is a valid one for a given player.
isMoveValid::Game->Role->Cell->Bool
isMoveValid g p cell =
  let cellIndex = gridcol_game * ((fst cell) -1) + (snd cell) -1    ---- cellIndex starts from 0.
  in if cellIndex > ((length g) -1) then False
      else if ((g !! cellIndex) == emptyCell) && (isbottomCell g cellIndex) then True
       else False

-- playMove makes a move to a cell and returns the new game state. This functions is called for human Role moves.
playMove::Game->Role->Cell->Game
playMove g p cell =  frontList ++ [p] ++ (tail afterwardsList)
    where cellIndex = (gridcol_game * ((fst cell) -1)) + ((snd cell) -1)
          (frontList, afterwardsList) = splitAt cellIndex g


-- moves function returns a list of possible moves/successor states that a player can make given a game state.
moves::Game->Role->[Game]
moves g p = map (\id -> let (frontList, afterwardsList) = splitAt id g in frontList ++ (p : tail afterwardsList))
   [i | i <- (elemIndices emptyCell g), isbottomCell g i]

-- checkWin function checks if the game state is a win for the player argument.
checkWin::Game->Role->Bool
checkWin g p = let occupiedCells = elemIndices p g --- useing predicate elemIndices to get the list of indexes of the celles occupied by the player.
    in if ([] /= (winLine getLines occupiedCells)) then True
        else False
        where winLine lines occuCells = [ l | l <- lines, (intersect l occuCells) == l]



-- getLines returns a list of all lines on the game board.
getLines :: [[Int]]
getLines =
    -- Horizontal
    reverse  [ [(r * gridcol_game + c + i) | i <- is]
    | r <- [0 .. gridrow_game - 1], c <- [0 .. gridcol_game - win_length]
    ] ++

    -- Vertical
    [ [((r + i)* gridcol_game +  c) | i <- is]
    | r <- [0 .. gridrow_game - win_length], c <- [0 .. gridcol_game - 1]
    ] ++

    -- Diagonal: top left to bottom right
    [ [((r + i) * gridcol_game + c + i) | i <- is]
    | r <- [0 .. gridrow_game - win_length], c <- [0 .. gridcol_game - win_length]
    ] ++

    -- Diagonal: bottom left to top right
    [ [((r + i)* gridcol_game + c - i) | i <- is]
    | r <- [0 .. gridrow_game - win_length], c <- [win_length - 1 .. gridcol_game - 1]
    ]
  where
    is = [0 .. win_length - 1]

-- draws and displays the board of game state on the screen.
drawGrid::Game->IO()
drawGrid g = go 0 []
    where go rowth gridString  =
           if (rowth >= gridrow_game)
              then putStrLn (unwords (("\n  " : [" C" ++ show(i)| i<-[1..gridcol_game]]) ++ ["\n"]) ++ gridString ++ "\n ") ::IO ()
              else go (rowth +1) (gridString  ++ "R" ++ show(rowth +1) ++ " |  " ++ drawLine g (rowth +1))
          drawLine g rowth= (unwords [drawPlayerMark(g !! (j))++ "| " |  j<- [(gridcol_game * (rowth-1)).. (rowth * gridcol_game-1)]]) ++ unwords(["\n   "] ++ ["---"| i<-[1..gridcol_game]] ++ ["\n"])

-- defines the 'x' and 'o' marks that are placed on the game board for max and min players
drawPlayerMark::Int->String
drawPlayerMark mark
        | mark == humanPlayer  = "x"
        | mark == compPlayer  = "o"
        | otherwise  = " "
