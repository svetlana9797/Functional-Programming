--Дефинирайте функция factorial, пресмятаща n!
factorial 0 = 1
factorial n = n* factorial(n-1)

factorial2 n
    |n==0 = 1
    |otherwise = n* factorial2(n-1)

--Дефинирайте функция fibonacci, която намира n-тото число на Фибоначи
fibonacci n
    |n==0 = 0
    |n==1 = 1
    |otherwise = fibonacci(n-1) + fibonacci(n-2)

--Двумерен вектор представяме чрез наредена двойка числа. 
--Дефинирайте функция addVectors, която събира два двумерни вектора.
addVectors [] [] = []
addVectors x [] = x
addVectors [] y = y
addVectors (x:xs) (y:ys) = (x+y): addVectors xs ys

addVectors2 (x:xs:[]) (y:ys:[]) = (x+y):(xs+ys):[]

--Дефинирайте функция compress, която премахва последователните повторения в даден списък.
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x==y = compress (y:xs)
    |otherwise = x : compress (y:xs)
--Дефинирайте функция duplicate, която повтаря всеки от елементите на даден списък
duplicate [] = []
duplicate (x:xs) = x:x: duplicate xs

--Напишете функция cycle, която съставя безкраен списък, повтаряйки циклично даден списък.
cycle' [] = []
cycle' xs = iter xs
    where iter [] = iter xs
          iter (x:xs) = x:iter xs

--Дефинирайте функция quickSort, която сортира даден списък, използвайки едноименния алгоритъм
quickSort [] = []
quickSort (pivot:xs) = 
    let smaller = [x| x<-xs, x<=pivot]
        bigger = [x| x<-xs, x>pivot]
    in quickSort smaller ++ [pivot] ++ quickSort bigger
--Дефинирайте функция mergeSort, която сортира даден списък, използвайки едноименния алгоритъм.
--Използвайте функцията splitAt
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where mid = length xs `quot` 2
        (firstHalf, secondHalf) = splitAt mid xs
        merge [] xs = xs
        merge xs [] = xs
        merge (x:xs) (y:ys)
          | y < x = y : merge (x:xs) ys
          | otherwise = x : merge xs (y:ys)

--Напишете собствена имплементация на стандартните функции zip и unzip
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x:y) : zip' xs ys

unzip' [] = ([],[])
unzip' xs = ( firsts, seconds)
    where firsts = map fst xs
          seconds = map snd xs

--Предложете реализация на стандартната функция reverse
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]
--Предложете реализация на стандартната функция iterate
iterate' f n = res : iterate' f res
    where res = f n
--Да се дефинира процедура (sum start end), която намира сумата на целите числа в интервала [start, end],
-- където start и end са цели числа.
sum' start end
    | start>end = 0
    | otherwise = start + sum' (start + 1) end
--Да се дефинира процедура (expt x n), която пресмята xn, където n е цяло число
expt' x n
    | n ==0 = 1
    |otherwise = x * expt' x (n-1)
--Да се дефинира процедура (fast-expt x n), която пресмята xn, където n е цяло число, използвайки следното свойство:
--Aко n е четно число, то xn = (x(n/2))2.
fastexp x n
    |n==0 = 1
    | mod n 2 == 0 = (fastexp x (div n 2))^2
    | otherwise = x * (fastexp x (n-1))
--Да се дефинира процедура (factorial-iter n), която пресмята n! чрез линейна итерация
fact_iter n = helper 1 n
    where helper start end 
              |start > end = 1
              |otherwise = start * helper (start + 1) end
--Да се дефинира процедура (sum-iter start end), която пресмята сумата на целите числа в интервала [start, end]
sum_iter start end 
    | start  > end = 0
    | otherwise = start + sum_iter (start + 1) end

--Да се дефинира процедура prime' n, която проверява дали числото n е просто
prime' 2 = True
prime' n = helper 2 n 
    where helper start num 
              | mod num start == 0 = False
              | start > (div num 2) = True
              | otherwise = helper (start +1) num
--Да се дефинира процедура (count-divisors n), която намира броя на делителите на числото n.
count_divisors n = helper 1 n
    where helper x n
              |x>n = 0
              |mod n x == 0 = 1 + helper (x+1) n
              |otherwise  = helper (x+1) n
--Да се дефинира процедура (sum-divisors n), която намира сумата на делителите на числото n
sum_divisors n = helper 1 n
    where helper x n 
              | x>n = 0
              |mod n x ==0 = x + helper (x+1) n
              |otherwise = helper (x+1) n