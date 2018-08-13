--problem 1
toList::Int->[Int]
toList 0 = []
toList k = mod k 10 : toList (div k 10)

productBy2::[Int]->[Int]
productBy2 xs = helper 1 xs (length xs) where
  helper _ [] _ = []
  helper i (x:xs) n
    |i>n = []
    |mod i 2 == 0 = (x*2) : helper (i+1) xs n
    |otherwise =  x : helper (i+1) xs n

sumDigits::Int->Int
sumDigits 0 = 0
sumDigits k = mod k 10 + sumDigits (div k 10)

calcLuhnChecksum ::Int->Int
calcLuhnChecksum k = mod res 10 where
  res = 9*sum (map sumDigits (productBy2(reverse(toList k))))
