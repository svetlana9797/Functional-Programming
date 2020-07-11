--Дефинирайте процедура (dimensions matrix), която връща наредена двойка с броя редове и броя колони на матрицата matrix.
dimensions [] = (0,0)
dimensions (x:xs) = (length (x:xs),length x)
--Дефинирайте процедура (reverse-columns matrix), която обръща реда на колоните в матрицата matrix.
rev [] = []
rev (x:xs) = rev xs ++ [x]

reverse_columns l = map rev l
--Дефинирайте процедура (nth-column matrix n), която връща списък с елементите на n-тата поред колона от матрицата matrix.
getcolumn n l = l!!(n-1)

nth_column matrix n = map (getcolumn (n)) matrix

--Дефинирайте процедура (main-diagonal matrix), която връща списък с елементите в главния диагонал на матрицата matrix.
main_diagonal matrix = helper 0 (length matrix) matrix 
    where helper _ _ [] = []
          helper a b m
              |a>=b = []
              |otherwise = [(m!!a)!!a] ++ helper (a+1) b m
--Дефинирайте процедура (transpose matrix), която връща транспонираната матрица на матрицата matrix.
--getelem _ [] = 0
getelem n l = l!!n

--mytake n m = map (getelem n) m
transpose m = helper 0 (length (m!!0)) m 
    where helper _ _ [] = []
          helper a b m 
              |a>=b = []
              |otherwise = [map (getelem a) m] ++ helper (a+1) b m

--Дефинирайте процедура (for-all-columns? p matrix), която проверява дали за всяка колона в матрицата matrix е изпълнен предикатът p
lt n = if n<10 then True else False

forallcolumns p m =and(map and(map (map lt) (transpose m)))

--Дефинирайте процедура (prime-in-each-column? matrix), която проверява дали във всяка колона в матрицата matrix има просто число.
isprime n 
    |n<=1 = False
    |otherwise = helper 2 n 
        where helper a b 
                 |a>=b = True
                 |mod b a == 0 = False
                 |otherwise = helper (a+1) b

hasprime [] = False
hasprime (x:xs)
    |isprime x ==True = True
    |otherwise = hasprime xs

prime_column m = and$map hasprime $ transpose m
