-- Inf2d Assignment 1 2017-2018
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import ConnectFour

gridLength_search :: Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the 3pm Tuesday 13th March 2018.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]



next :: Branch-> [Branch]
next [] = []
next br =  [(x,y):br | pos <-[(1,0),(0,1),(-1,0),(0,-1)], x<-[fst(head br) + fst(pos)], y<-[snd(head br) + snd(pos)], notElem (x,y) br, x>=1,x<=6,y>=1,y<=6]


-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival :: Node-> Node-> Bool
checkArrival destination curNode = destination==curNode

manhattan::Node-> Node-> Int
manhattan position destination = x + y
  where
    x = abs (fst(position) - fst(destination))
    y = abs (snd(position) - snd(destination))


breadthFirstSearch :: Node-> (Branch -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
breadthFirstSearch destination next branches exploredList
      | null branches = Nothing
      | elem True results = Just (head [x | x <- branches, checkArrival destination (head x)])
      | otherwise = breadthFirstSearch destination next next_branches nextList
      where results = [checkArrival destination (head x) | x <- branches]
            nextList = [(head x) | x <- branches, notElem (head x) exploredList] ++ exploredList
            next_branches = [x |x <- concatMap next branches, notElem (head x) exploredList]

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next branches exploredList
    | found = Just (head branches)
    | null branches = Nothing
    | null (next (head branches)) = depthFirstSearch destination next recheckBranch nextList
    | otherwise = depthFirstSearch destination next next_branch nextList
        where
          found = checkArrival destination (head (head branches))
          nextList = [head (head branches)] ++ exploredList
          next_branch = [x | x<-next (head branches), notElem (head x) exploredList] ++ branches
          recheckBranch = [x | x <- (drop 1 branches)]


depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next branches d exploredList
    | null branches = Nothing
    | found = Just (head branches)
    | d<1 = depthLimitedSearch destination next (drop 1 branches) d exploredList
    | null (next (head branches)) = depthLimitedSearch destination next (drop 1 branches) d nextList
    | otherwise = depthLimitedSearch destination next next_branch (d-1) nextList
        where
          found = checkArrival destination (head (head branches))
          nextList = [head x | x<- branches, notElem (head x) exploredList] ++ exploredList
          next_branch = [x | x<-next (head branches), notElem (head x) exploredList] ++ branches

bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
    | null branches = Nothing
    | not (null list) = Just (head list)
    | otherwise = bestFirstSearch destination next heuristic nextBranches nextList
        where
          list = [x | x<-branches, checkArrival destination (head x)]
          heads = [head y | y <- branches, notElem (head y) exploredList]
          minHeurVal = minimum [x | x<- map heuristic heads]
          minBranch = head [x | x <- branches, (heuristic (head x)) == minHeurVal]
          nextBranches = [x | x<-next(minBranch), notElem (head x) exploredList]
          nextList = [head x | x<- branches, notElem (head x) exploredList] ++ exploredList

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
    | null branches = Nothing
    | not (null list) = Just (head list)
    | otherwise = aStarSearch destination next heuristic cost nextBranches nextList
        where
          list = [x | x<-branches, checkArrival destination (head x)]
          added = [(cost y)+(heuristic (head y)) | y<-branches, notElem (head y) exploredList]
          minBr = head [x | x<- branches, ((cost x)+(heuristic (head x)))== (minimum added)]
          nextBranches = [x | x<-next(minBr), notElem (head x) exploredList] ++ branches
          nextList = [head x | x<-branches, notElem (head x) exploredList] ++ exploredList

cost :: Branch-> Int
cost branch = length branch

eval :: Game-> Int
eval game
  | checkWin game humanPlayer = 1
  | checkWin game compPlayer = (-1)
  | otherwise = 0

minimax:: Role-> Game-> Int
minimax player game
  | terminal game = eval game
  | player==maxPlayer = maximum [minimax (switch player) maxmovegame]
  | player==minPlayer = minimum [minimax (switch player) minmovegame]
    where
      playerevalMoves = [eval x | x<-(moves game player)]
      minmovegame = head [x | x<-(moves game player), eval x==(minimum playerevalMoves)]
      maxmovegame = head [x | x<-(moves game player), eval x==(maximum playerevalMoves)]

--minimax:: Role-> Game-> Int
--minimax player game
--  | player==maxPlayer = maximum (getAllPos player gameList emptyList)
--  | player==minPlayer = minimum (getAllPos player gameList emptyList)
--   where
--     gameList = [game]
--     emptyList = []

--getAllPos:: Role-> (Role->[Game]->[Game]) ->[Int]-> [Int]
--getAllPos player moves games list
--  | length newGames == length games = list
--  | otherwise = getAllPos (switch player) moves games newList
--  where
--     newGames = [(moves player x) | x<- games, notElem (moves player x) games]
--     newList = [eval x | x<-games, terminal x] ++ list
