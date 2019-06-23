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


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next :: Branch-> [Branch]
next [] = []
-- the next function takes a branch and its head, seeing as we are adding nodes to branches at the head of a brnch, and expands said branch to include the adjacent nodes,
-- as long as they are not already in the branch being observed, and as long as x and y fit the constraints.
next br =  [(x,y):br | pos <-[(1,0),(0,1),(-1,0),(0,-1)], x <-[fst(head br) + fst(pos)], y <-[snd(head br) + snd(pos)], notElem (x,y) br, x>=1, x<=6, y>=1, y<=6]


-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
-- It compares both nodes and returns true only if they are equal.
checkArrival :: Node-> Node-> Bool
checkArrival destination curNode = if (destination==curNode) then True else False


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

-- The Breadth-First Search works in the following ways: when our search agenda is empty, that means we have gone through everything and not found  solution, so we return Nothing.
-- results is a list of the results of the checkArrival function being applied to the last node of the branches in the search agenda, and so if there is a true value, we print that branch.
-- if not, we recursively continue the search by altering the next search agenda branches to be the continuation of branches, done with the next function, and the list to include the nodes searched this time.

breadthFirstSearch :: Node-> (Branch -> [Branch]) -> [Branch] -> [Node] -> Maybe Branch
breadthFirstSearch destination next branches exploredList
    | null branches = Nothing
    | elem True results = Just (head [x | x <-branches, checkArrival destination (head x)])
    | otherwise = breadthFirstSearch destination next next_branches nextList
        where
           results = [checkArrival destination (head x) | x <-branches]
           nextList = [head x | x <-branches, notElem (head x) exploredList] ++ exploredList
           next_branches = [x |x <-concatMap next branches, notElem (head x) exploredList]


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

-- The Depth-First Search works in the following ways: when our search agenda, branches, is empty, that means we have gone through everything and not found the solution, so we return Nothing.
-- found is a constant check of the first node of each child, and if true we have found the solution branch and return that.
-- if the result of applying next to the first child branch does not give any results, an empty list, then we continue the search but dropping one from branches, so we move on to the next available place to search
-- this is how to ensure when a child has no continuation, you can go to the next child and continue search, doing this recursively, and also using the nextList, where we have added the newly explored nodes.
-- if no solution found, if child has more children, then we keep using the search with the nextList and the next_branch, the result of the next function applied to branches.

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next branches exploredList
    | found = Just (head branches)
    | null branches = Nothing
    | null (next (head branches)) = depthFirstSearch destination next (drop 1 branches) nextList
    | otherwise = depthFirstSearch destination next next_branch nextList
      where
        found = checkArrival destination (head (head branches))
        nextList = [head x | x <-branches, notElem (head x) exploredList] ++ exploredList
        next_branch = [x | x <-next (head branches), notElem (head x) exploredList] ++ branches

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree..

-- the Depth-Limited Search works in the following ways : it follows the same process as depth-First Search, except it also keeps track of the depth explored (d decreases as you go down, reaches 0 when should not keep going),
-- if depth explored reaches the d given and no solution has been found, we return nothing. the found boolean works in the same way here.
-- If d is less than 1, aka 0, we keep searching but with the next explorable branch in branches at the same depth. If there are no continuations to that child, we do the same.
-- however, if you can keep going, we simply keep the search goind with (d-1) as a parameter now, to show we are deepening the search.
-- nextList adds new unexplored nodes to the already existing list, and next branch expands the branches in branches with the next function.

depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next branches d exploredList
    | null branches = Nothing
    | found = Just (head branches)
    | d<1 = depthLimitedSearch destination next (drop 1 branches) d nextList
    | null (next (head branches)) = depthLimitedSearch destination next (drop 1 branches) d nextList
    | otherwise = depthLimitedSearch destination next next_branch (d-1) nextList
      where
        found = checkArrival destination (head (head branches))
        nextList = [head x | x <-branches, notElem (head x) exploredList] ++ exploredList
        next_branch = [x | x <-next (head branches), notElem (head x) exploredList] ++ branches

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.

-- the Iterative-deepening search works very similarly to the depth-LimitedSearch, except if an answer is not found at depth d, this algorithm
-- simply adds 1 to d and repeats the process, and will keep adding 1 to d until it does.  If it is found, then the result given by the depthLimitedSearch with those inputs is the answer.
iterDeepSearch:: Node-> (Branch-> [Branch])-> Node-> Int-> Maybe Branch
iterDeepSearch destination next initialNode d
    | (depthLimitedSearch destination next [[initialNode]] d [])==Nothing  = iterDeepSearch destination next initialNode (d+1)
    | otherwise = depthLimitedSearch destination next [[initialNode]] d []

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the current position and the destination position.

-- We achieve this by finding the absolute value of the difference between first and second positions, then add the absolutes.
manhattan::Node-> Node-> Int
manhattan position destination = x + y
  where
    x = abs (fst(position) - fst(destination))
    y = abs (snd(position) - snd(destination))


-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

-- the bestFirstSearch uses a heuristic, instead of order, to prioritise branches in the search agenda. Again, if the search agenda is empty at any point, we have not found a solution and return Nothing.
-- if list is not empty, where list includes the branches from the search agenda if the first and lastly explored node is the destination, this means we have a solution, so we return the first solution in the list.
-- however if it is empty, meaning we have not yet found a solution, we proceed with the search with nextBranches and nextList. NextList acts as previously, while nextBranches acts in the following way∷
-- we find the minimum valued branch using the heuristic, and then make the new search agenda, nextBranches, the explored version of that minimum valued branch.
-- If you do this recursively you should always find the minimum choice at each step, achieving the bestFirstSearch.
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
    | null branches = Nothing
    | not (null list) = Just (head list)
    | otherwise = bestFirstSearch destination next heuristic nextBranches nextList
        where
          list = [x | x <-branches, checkArrival destination (head x)]
          heads = [head y | y <-branches, notElem (head y) exploredList]
          minHeurVal = minimum [x | x <-map heuristic heads]
          minBranch = head [x | x <-branches, (heuristic (head x))==minHeurVal]
          nextBranches = [x | x <-next(minBranch), notElem (head x) exploredList]
          nextList = [head x | x <-branches, notElem (head x) exploredList] ++ exploredList


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

-- the astarSearch function works very similarly to bestFirstSearch except it also takes into account the current cost of each branch in the search agenda.
-- By using the heuristic you can choose which option is better, but by using cost you also factor in if the cost is great to get to that branch. Combining them, where
-- choose to explore the branch with the lowest combination of the two.

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
    | null branches = Nothing
    | not (null list) = Just (head list)
    | otherwise = aStarSearch destination next heuristic cost nextBranches nextList
      where
         list = [x | x<-branches, checkArrival destination (head x)]
         added = [(cost y)+(heuristic (head y)) | y <-branches]
         minBranch = head [x | x <- branches, ((cost x)+(heuristic (head x)))==(minimum added), notElem (head x) exploredList]
         nextBranches = [x | x <-next(minBranch), notElem (head x) exploredList] ++ branches
         nextList = [head x | x <-branches, notElem (head x) exploredList] ++ exploredList


-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch-> Int
cost [] = 0
cost branch = length branch

-- In this section, the function determines the score of a terminal state, assigning it a value of +1, -1 or 0:

-- using the checkWin function provided, we check for each player with that game.
eval :: Game-> Int
eval game
   | checkWin game humanPlayer = 1
   | checkWin game compPlayer = (-1)
   | otherwise = 0


-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.
minimax:: Role-> Game-> Int
minimax player game
    | terminal game = eval game
    | player==maxPlayer = maximum' [minimax (switch player) maxmovegame]
    | player==minPlayer = minimum' [minimax (switch player) minmovegame]
      where
         playerevalMoves = [eval x | x<-(moves game player), terminal x]
         minmovegame = head [x | x<-(moves game player), eval x==(minimum' playerevalMoves)]
         maxmovegame = head [x | x<-(moves game player), eval x==(maximum' playerevalMoves)]

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Role-> Game-> Int
alphabeta  player game  = alphabetaF game player alpha beta
  where alpha = - (infinity)
        beta = infinity

alphabetaF:: Game-> Role-> Int -> Int -> Int
aphabetaF game player alpha beta
   | terminal game = eval game
   | v>=infinity = v
   | v<=(- infinity) = w
     where v = max (-infinity, minimum' (minimax player game))
           w = min (infinity, maximum' (minimax player game))



{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}
-- this mximum function ensures you get a 0 if empty, and not an error, same for the minimum
maximum',minimum' :: [Int] -> Int
maximum' [] = 0
maximum' [x] = x
maximum' (x:xs)
  | x > maximum' xs = x
  | otherwise = maximum' xs

minimum' [] = 0
minimum' [x] = x
minimum' (x:xs)
  | x > minimum' xs = x
  | otherwise = minimum' xs
