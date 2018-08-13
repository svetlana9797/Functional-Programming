--problem 2
--returns a list with all the possible positions
possiblePositions :: (Int,Int)->[(Int,Int)]
possiblePositions (x,y) = (x+1,y-1):(x+1,y):(x+1,y+1):(x,y-1):(x,y+1):(x-1,y-1):(x-1, y):(x-1, y+1):[]

--flatten a list
flatten::[[(Int,Int)]]->[(Int,Int)]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

--returns the number of alive neighbours of a cell
aliveNeighbours::(Int,Int)->[(Int,Int)]->Int
aliveNeighbours cell xs = helper (possiblePositions cell) xs where
  helper []  _ = 0
  helper (p:ps) xs
    |elem p xs = 1 + helper ps xs
    |otherwise = helper ps xs

--returns wheter a cell is alive or not
isAlive::(Int,Int)->[(Int,Int)] -> Bool
isAlive x xs 
    |n==2 || n==3 = True
    |otherwise = False
    where n = aliveNeighbours x xs

--returns whether a cell should be born or not
isBorn::(Int,Int)->[(Int,Int)] -> Bool
isBorn x xs
    |n==3 = True
    |otherwise = False
    where n = aliveNeighbours x xs

--removes an element from the list exept for the first meeting
clear::(Int,Int)->[(Int,Int)] ->[(Int,Int)]
clear _ [] = []
clear y (x:xs)
    |x==y = clear y xs
    |otherwise = x: clear y xs

--clears the list from duplicate elements
clearFromDuplicates :: [(Int,Int)]-> [(Int,Int)]
clearFromDuplicates [] = []
clearFromDuplicates (x:xs)
    |elem x xs = x:clearFromDuplicates (clear x xs)
    |otherwise = x : clearFromDuplicates xs

--clears a list from the elements of the list xs
clearElementsFromList::[(Int,Int)]->[(Int,Int)]->[(Int,Int)]
clearElementsFromList xs [] = []
clearElementsFromList xs (y:ys)
    |elem y xs = clearElementsFromList xs ys
    |otherwise = y : clearElementsFromList xs ys

--returns a list with the newborn cells
bornCells::[(Int,Int)]->[(Int,Int)]
bornCells xs = helper xs (clearElementsFromList xs (clearFromDuplicates (flatten (map possiblePositions xs)))) where
  helper _ [] = []
  helper [] _ = []
  helper l (p:ps)
    |isBorn p l = p : helper l ps
    |otherwise = helper l ps

--removes the dead cells
removeDead::[(Int,Int)]->[(Int,Int)]
removeDead [] = []
removeDead l = helper l l where
  helper [] _ = []
  helper (x:xs) xss
    |isAlive x xss = x : helper xs xss
    |otherwise = helper xs xss

gameOfLife::[(Int,Int)] ->[(Int,Int)]
gameOfLife [] = []
gameOfLife board = (removeDead board) ++ (bornCells board)
