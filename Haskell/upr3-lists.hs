--Да се дефинира процедура (length l), която намира броя на елементите на списъка l
len [] = 0
len (x:xs) = 1 + len xs

--Да се дефинира процедура (sum l), която намира сумата на елементите на списъка l
s [] = 0
s (x:xs) = x + s xs

--Да се дефинира процедура (mem x l), която проверява дали x е елемент на списъка l
mem x [] = False
mem x (y:ys)
    |x==y = True
    |otherwise = mem x ys

--Да се дефинира процедура (last l), която връща последния елемент на списъка l
last' [x] = x
last' (x:xs)  = last' xs

--Да се дефинира процедура (nth l n), която връща елемента на позиция n, броейки от 0, в списъка l.
nth _ [] = []
nth 0 (x:xs) = [x]
nth n (x:xs) = nth (n-1) xs

--Да се дефинира процедура (scale l x), която връща списък с елементите на списъка l, умножени по числото x
scale [] _ = []
scale (x:xs) k = [x*k] ++ scale xs k

--Да се дефинира процедура (reverse l), която връща списък, чиито елементи са елементите на списъка l в обратен ред.
rev [] = []
rev (x:xs) = rev xs ++ [x]

--Дa се дефинира процедура (add-last l x), коята добавя елемент x на края на списъка l
add_last [] x = [x]
add_last l x = l ++ [x]

--Да се дефинира процедура (append l1 l2), която конкатенира списъците l1 и l2
append' [] l = l
append' x [] = x
append' x (l:ls) = append' (x ++ [l]) ls

--Да се дефинира процедура (map f l), която прилага едноаргументната процедура f върху всеки елемент на списъка l
map' f [] = []
map' f (x:xs) = [f x] ++ map' f xs

--Да се дефинира процедура (filter p l), която връща списък с елементите на списъка l, които удовлетворяват предиката p.
filter' p [] = []
filter' p (x:xs)
    |p x == True = [x] ++ filter' p xs
    |otherwise = filter' p xs

--Процедури (every? p l) и (any? p l), проверяващи съответно дали за всеки елемент на списъка l е изпълнен предикатът p 
--и дали съществува елемент от списъка l, за който е изпълнен предикатът p.
every' p [] = True
every' p (x:xs)
    | p x == False = False
    |otherwise = every' p xs

any' p [] = False
any' p (x:xs)
    | p x == True = True
    |otherwise = any' p xs

--Процедура (enumerate-interval from to), която връща списък от целите числа в интервала [from, to],
enum_interval a b
    |a>b = []
    |otherwise = [a] ++ enum_interval (a+1) b

--take-while връща списък от последователните елементи от началото на списъка l, за които предикатът p е изпълнен

takewhile _ [] = []
takewhile p (x:xs)
    |p x == False = []
    |otherwise = [x] ++ takewhile p xs

--drop-while връща списък с елементите от списъка l без последователните елементи от началото на l, за които предикатът p е изпълнен. 
dropwhile _ [] = []
dropwhile p (x:xs)
    |p x == False = x:xs
    |otherwise = dropwhile p xs

--Процедура (selection_sort l), която сортира списъка l по метода на пряката селекция
min' a b
    |a<=b = a
    |otherwise = b

--find the minimum element from the list
findmin [x] = x
findmin (x:xs) = min' x (findmin xs)

--remove the first occurrence of the element x from the list 
remove _ [] = []
remove x (y:ys)
    |x==y = ys
    |otherwise = [y]++ remove x ys

selection_sort l = helper [] l
    where helper res [] = res
          helper res xs = helper (res ++ [min_el]) (remove min_el xs)
              where min_el = findmin xs

--Процедура (quicksort l), която сортира списъка l чрез алгоритъма "бързо сортиране" (quicksort)
quicksort [] = []
quicksort (x:xs) = quicksort(smaller) ++ [x] ++ quicksort(bigger)
    where smaller = [y|y<- xs, y<=x]
          bigger = [y|y<-xs, y>x]

--Процедура (prime-sum-pairs n), която по дадено цяло положително число n намира всички наредени тройки (i, j, i + j), за които:
--i и j са цели положителни числа,
--1 ≤ j < i ≤ n 
--i + j е просто.
isprime n = helper 2 n 
    where helper i n
              |i==n = True
              |rem n i == 0 = False
              |otherwise = helper (i+1) n

primesumpairs i j n 
    |i>n || j>n || j>=i = []
    |isprime (i+j) == True = [[i,j,i+j]] ++ primesumpairs i (j+1) n
    |otherwise = primesumpairs i (j+1) n

prime_sum_pairs n 
    |n<2 = []
    |otherwise = helper 1 n
        where helper i n 
                  |i>n = []
                  |otherwise = primesumpairs i 1 n ++ helper (i+1) n

--Да се напише процедура (middle-digit n), която намира средната цифра от записа на подадено естествено число n. Ако n е с четен брой цифри, процедурата да връща -1. 
kth_digit k n
    |k==0 = mod n 10
    |otherwise = kth_digit (k-1) (div n 10)

len' n
    |n<10 = 1
    |otherwise = 1+ len' (div n 10)

middledigit n
    |mod (len' n) 2==0 = -1
    |otherwise = kth_digit (div (len' n) 2) n

--Да се напише процедура (meet-twice? f g a b), която проверява дали в целочисления интервал [a, b] съществуват две различни цели числа x и y такива, че f(x) = g(x) и f(y) = g(y).
isin _ [] = False
isin x (y:ys)
    |(head x == last y) && (head y == last x) = True
    |otherwise = isin x ys

hasduplicates [] = False
hasduplicates [x] = False
hasduplicates (x:xs)
    |isin x xs == True = True
    |otherwise = hasduplicates xs

helper f g a b l 
    |a>b = False
    |hasduplicates l == True = True
    |otherwise = helper f g (a+1) b (l ++ [[f a, g a]])

meettwice f g a b = helper f g a b []

--Казваме, че списъкът x = (x1 x2 … x2n) от цели числа се получава от прочитането (look-and-say) на списъка y, ако y се състои от последователно срещане на x1 пъти x2, последвано от x3 пъти x4 и така нататък до x2n-1 пъти x2n.
duplicates _ [] = 0
duplicates x (y:ys)
    |x/=y = 0
    |otherwise = 1 + duplicates x ys

until' _ [] = []
until' x (y:ys) 
    |x/=y = (y:ys)
    |otherwise = until' x ys

lookandsay [] =[]
lookandsay (x:xs) = [duplicates x (x:xs)] ++ [x] ++ lookandsay(until' x (x:xs))