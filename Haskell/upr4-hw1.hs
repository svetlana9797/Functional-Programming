--Дефинирайте процедура (repeat value), която връща безкраен поток, който генерира стойности value.
rep::t->[t]
rep value = value : (rep value)

--Предложете реализация на стандартната функция iterate
iter::(t->t)->t->[t]
iter f x = f x : (iter f (f x))

--Дефинирайте процедура (cycle l), която връща безкраен поток, който генерира елементите на списъка l
cycle'::t->[t]
cycle' x = x:(cycle' x)

--Да се напише функция (convert x k n), която получава цяло число x ≥ 0,в k-ична позиционна бройна система, и връща числото в n-ична бройна система, където 2 ≤ k, n ≤ 10
convert x k n
    |x==0 = 0
    |otherwise = rem x n + k*(convert (div x n) k n)

--Да се напише функция sum-numbers, която приема един аргумент – символен низ и връща сумата на всички числа в него.
isDigit::Char->Bool
isDigit x = elem x [y|y<-['0','1','2','3','4','5','6','7','8','9']] -- сравняваме стринговете

takenumber::[Char]->[Char]
takenumber [] =[]
takenumber (x:xs)
    |isDigit x = x:(takenumber xs)
    |otherwise = []

cutnumber [] = []
cutnumber (x:xs)
    |isDigit x == False = (x:xs)
    |otherwise = cutnumber xs

sumnum [] = 0
sumnum (x:xs)
    |isDigit(x) = (read (takenumber(x:xs)) ::Int) + sumnum(cutnumber xs)
    |otherwise = sumnum xs

--Да се напише функция encode, която компресира даден списък.
countelem x [] = 0
countelem x (y:ys)
    |x==y = 1 + countelem x ys
    |otherwise = 0

countchar x y = (x, countelem x y)

removechar _ [] = []
removechar x (y:ys)
    |x/=y = (y:ys)
    |otherwise = removechar x ys

encode [] = []
encode (x:xs) = (countchar x (x:xs)) : encode (removechar x xs)

-- Да се напише функция maximize, която получава непразен списък от
--едноместни числови функции и връща нова едноместна числова функция на
--аргумент x, която връща стойността f(x) на тази фунция f от списъка, за която
--числото f(x) е най-голямо по абсолютна стойност.


maximize (f:fs) x = helper (f x) (f:fs) x
    where helper fx [] _ = fx
          helper fx (f:fs) x
              |abs(fx) < abs(f x) = helper (f x) fs x
              |otherwise = helper fx fs x

--Да се дефинира функция (divides-all numbers), която приема списък от цели числа numbers и
--връща най-малкото положително цяло число, което се дели без остатък на всеки от елементите на numbers
sortelem [] = []
sortelem (x:xs) = sortelem (smaller) ++[x] ++ sortelem(bigger)
    where smaller = [y|y<-xs, y<=x]
          bigger = [y|y<-xs, y>x]

dividesall _ [] = True
dividesall x (y:ys)
    |mod y x /=0 = False
    |otherwise = dividesall x ys

divallnums [] = 0
divallnums (x:xs) = helper (sortelem (x:xs)) (x:xs)
    where helper [] _ = 0
          helper (x:xs) l
              |dividesall x l && x>0 = x 
              |otherwise = helper xs l

--Да се дефинира функция (set-union xs ys), която връща обединението на множествата от
--числа xs и ys, представени като списъци, наредени във възходящ ред
setunion [] [] = []
setunion [] ys = sortelem ys
setunion xs [] = sortelem xs
setunion (x:xs) (y:ys)
    |x<y = [x] ++ setunion xs (y:ys)
    |otherwise = [y] ++ setunion (x:xs) ys 

remv _ [] = []
remv x (y:ys)
    |x==y = remv x ys
    |otherwise = [y] ++ remv x ys

removedup [] = []
removedup (x:xs)
    |elem x xs = [x] ++ removedup (remv x xs)
    |otherwise =[x] ++ removedup xs

setun xs ys = removedup(setunion xs ys)

-- Да се дефинира функция (both-in f g start end), чиито аргументи f и g са едноаргументни
--целочислени функции, а start и end са двата края на целочислен интервал. Функцията both-in да връща
--като резултат друга функция – едноместен предикат, чиято върната стойност е true (#t) за всички
--стойности на аргумента x, за които стойностите (f x) и (g x) са в интервала [start, end].
in_interval f start end x 
    |f x >=start &&f x <=end = True
    |otherwise = False

both_in f g start end = (\x-> if in_interval f start end x && in_interval g start end x then True else False)
    