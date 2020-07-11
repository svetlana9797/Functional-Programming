--Дефинираме функция f по следните правила:
--f(n) = n, ако n < 3
--f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3), ако n ≥ 3
--Напишете процедура, пресмятаща f

f n
    | n<3 = n
    | otherwise = (f (n-1)) + 2* (f (n-2)) + 3* (f (n-3))

--Напишете процедура (count predicate a b), която връща броя на целите числа в интервала [a, b], за които predicate е истина.
counter predicate a b 
    | a >= b = 0
    | predicate a == True = 1 + counter predicate (a+1) b
    | otherwise = counter predicate (a+1) b


--Напишете процедура, която връща броя на палиндромите в даден интервал.
number_digits x
    | x<10 = 1
    | otherwise = 1 + number_digits (div x 10)

reverse_num x = helper (number_digits x) x
    where helper n x
             | x<10 = x
             | otherwise = (mod x 10) * (10 ^(n-1)) + helper (n-1) (div x 10)

palindrome x = x == (reverse_num x)

count_palindromes a b 
    | a>b = 0
    | palindrome a == True = 1 + count_palindromes (a+1) b
    | otherwise = count_palindromes (a+1) b

--Напишете процедура (for-all? predicate a b), която проверява дали за всяко цяло число в интервала [a, b] predicate е истина.
forall predicate a b
    | a>b = True
    | predicate a == False = False
    | otherwise = forall predicate (a+1) b

--Напишете процедура (double f), която връща композицията f ∘ f, където f е едноаргументна процедура.
double f x = f(f x)

-- Напишете процедура (compose f g), която връща композицията f ∘ g
compose f g x = f(g x)

--Напишете процедура (repeated f n), която връща n-тото прилагане на f.
repeated f n x
    | n == 1 = f x
    | otherwise = f (repeated f (n-1) x)
