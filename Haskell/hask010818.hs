import Data.Char
import Data.List

fastxpt x n  = if n==0
                then 1
				else if (mod n 2)==0 
                     then fastxpt (x^2) (div n 2)
				     else x * fastxpt x (n-1)

--Да се дефинира функция elem(x, l),
-- която проверява дали елемента x присъства в списъка l
myElem::(Eq t)=>t->[t]->Bool
myElem x [] = False
myElem x l@(y:yl)
    |x==y = True
    |otherwise = elem x yl
	
--Да се дефинира функция reverse(l), която обръща списъка l
--using pattern matching
myReverse :: [t]->[t]
myReverse (x:xs)
    |null xs = [x]
    | otherwise = myReverse xs ++ [x]

--without pattern matching	
myReverse2 :: [t]->[t]
myReverse2 l
    |null l = []
 --   |otherwise = [last l] ++ myReverse (init l)
	|otherwise = myReverse2 (tail l) ++[head l] 

square:: Int->Int
square x = x * x

add1 ::Int->Int
add1 x = x + 1

--map(f, l), която прилага функцията f върху всеки елемент от списъка l
myMap :: [t]->(t->t)->[t]
myMap [] _ = []
myMap (x:xs) f = [(f x)] ++ myMap xs f


isOdd :: Int->Bool
isOdd x = (mod x 2) == 1
         
		  
--filter(p, l), която връща списък
myFilter :: [t]->(t->Bool)->[t]
myFilter [] _ = []
myFilter (x:xs) p 
    | p x = [x] ++ myFilter xs p
    | otherwise = myFilter xs p
	
--дали числото к е делител на n
isDivider:: Int->Int->Bool
isDivider k n = (mod n k) ==0

max3:: Int->Int->Int->Int
max3 x y z = max x (max y z)

--проверка за просто число
isDivisible ::Int->[Int]->Bool
isDivisible _ [] = False
isDivisible n (x:xs)
    |mod n x == 0 = True
	|otherwise = isDivisible n xs
---
isPrime:: Int->Bool
isPrime 1 = False
isPrime x = not(isDivisible x [2..x-1])

--дали число е равно равни на сбора на 
--цифрите си на степен броя на цифрите си

numberOfDigits :: Int->Int
numberOfDigits 0 = 0
numberOfDigits x = 1 + (numberOfDigits (div x 10))

sumOfDigits ::Int->Int->Int
sumOfDigits x n
    |x==0 = 0
    |otherwise  = (mod x 10)^n + sumOfDigits(div x 10) n

isNarcissistic:: Int->Bool
isNarcissistic x = (sumOfDigits x (numberOfDigits x) == x)

--обръща цифрите на числото n
reverseDigits :: Int->Int
reverseDigits x  
    | x< 10 = x
    | otherwise = (mod x 10)*(10^nd) + reverseDigits (div x 10)
     where nd = (numberOfDigits x) - 1
	 
--Accumulate with recursion
accumulate::Ord t1 => (t2->t3->t3)->t3->(t1->t2)->t1->(t1->t1)->t1->t3
accumulate combiner nv term a next b
    |a>b = nv
    |otherwise  = combiner (term a) (accumulate combiner nv term (next a) next b)
	
--zip function
myZip :: [a]->[b]->[(a,b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys


--sorting
mySort:: (Ord t) => [t]->[t]
mySort [] = []
mySort (x:xs) =
  let smallerSorted = [y| y<-xs, y <= x]
      biggerSorted = [z| z<-xs, z > x]
  in (mySort smallerSorted) ++ [x] ++ (mySort biggerSorted)

--collatz numbers
collatz :: Int->[Int]
collatz x 
    |x==1 = [x]
    |mod x 2==0 = [x] ++ collatz res
	|otherwise = [x] ++ collatz res2
	where res = div x 2; res2 = x*3 + 1
	
{-numLongChains

numLongChains:: [Int]->Int
numLongChains [] = 0
numLongChains (x:xs) = if length(collatz x)>15
                       then 1+numLongChains xs
					   else numLongChains xs 
-}
					   
numLongChains :: Int  
numLongChains = length (filter isLong (map collatz[1..100]))  
    where isLong xs = length xs > 15 
colltz:: Int->Int
colltz x 
    |mod x 2==0 = div x 2
    |otherwise = x*3 + 1
	
areCollatzSequence :: [Int]->Bool
areCollatzSequence [x] = True
areCollatzSequence (x:y:xs) 
    | colltz x /=y = False
    |otherwise = areCollatzSequence (y:xs)
	
--повторение на елемент
times:: Int->t->[t]
times 0 x = []
times n x = x: (times (n-1) x)

--обединява 2 сортирани списъка
myMerge :: [Int]->[Int]->[Int]
myMerge l1 [] = l1
myMerge [] l2 = l2
myMerge l1@(x:xs) l2@(y:ys)
     |x<y  = x:myMerge xs l2
	 |otherwise = y: myMerge l1 ys


mergeSort ::[Int]->[Int]
mergeSort [] = []
mergeSort [x] =[x]
mergeSort xs = 
 let n = div(length xs) 2
 in myMerge (mySort(take n xs)) (mySort(drop n xs))
 
--Burrows-Wheeler transformation
myRotate:: Int->[t]->[t]
myRotate 0 xs = xs
myRotate n (x:xs) = (myRotate (n-1) (xs ++ [x]))
--
rotations' :: [t]->[[t]]
rotations' str = helper 0 (length str) where
  helper i n
    |i >= n = []
    |otherwise = (myRotate i str) : (helper (i+1) n)
--
bwt:: (Ord t)=>[t]->[t]
bwt str = map last (mySort (rotations' str))				

--
filterPrimePositions ::[t]->[t]
filterPrimePositions [] = []
filterPrimePositions xs = helper 0 (length xs) where
  helper i n 
    |i>= n = []
    |isPrime i ==True = (xs!! i):(helper (i+1) n)
	|otherwise = helper (i+1) n 
		
--countminimun
countMinimum :: (Ord a)=> [a]->Int
countMinimum xs = length(myFilter xs (== min_elem)) where
  min_elem = minimum xs

 --оправя главни и малки букви
myTitle :: String->String
myTitle str = helper str True where 
  helper "" _ = ""
  helper (x:xs) isSpacePrev 
    |isSpace x= x: helper xs True
	|isSpacePrev = (toUpper x) :helper xs False
    |otherwise = (toLower x) : helper xs False
	
--images
images:: (Double->Double)->(Double->Double)->[(Double,Double)]->[(Double,Double)]
images _ _ [] = []
images f g (x:xs)
    | f(fst x)==g(snd x) = x: images f g xs 
    |otherwise = images f g xs
	
--дали следващия елемент се получава от предишния чрез f
isIterator :: (Eq t) =>(t->t)->[t]->Bool
isIterator _ [x] = True
isIterator f (x:y:xs)
    |f(x)/=y = False
    |otherwise = isIterator f (y:xs)
	
--символен низ, който съдържа всеки символ от  азбуката
isPangram :: String->Bool
isPangram str = helper str ['a'..'z'] where
  helper (x:xs) ys 
    |null ys = True
    |null xs = False
    |elem (toLower x) ys = helper xs (delete x ys)
    |otherwise = helper xs ys
	