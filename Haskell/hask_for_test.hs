import Data.List
import Data.Function
--намира най-дългата верига намаляващи цифри в к
revNum ::Int->Int->[Int]->Int
revNum 0 res xs= maximum xs
revNum x res xs
    |ldig < mod pnum 10 = revNum pnum expr xs
    |otherwise = revNum pnum 0 (expr:xs)
    where ldig = mod x 10; pnum = div x 10 ; expr = res*10 +ldig

--връща най-дългият суфикс на к с намаляващи цифри
reverseOrdSuff::Int->Int
reverseOrdSuff k = helper k 0 where
  helper n res 
    |lg < mod pnum 10 = helper pnum expr
    |otherwise = expr
    where lg = mod n 10; pnum = div n 10; expr = res*10 + lg
	
--по списък от списъци намира сумата на уникалните числа в списъка
remove'::Int->[Int]->[Int]
remove' _ [] = []
remove' y (x:xs)
    |y==x = remove' y xs
    |otherwise = x: remove' y xs
	
findUnique::[Int]->[Int]
findUnique [] = []
findUnique (y:ys) 
    |elem y ys ==False = y: findUnique (remove' y ys)
    |otherwise = findUnique (remove' y ys)
	
sumUnique :: [[Int]] -> Int
sumUnique xs = sum(map sum (map findUnique xs))

--зад 3
type Product = (String,Double)
type StoreAvailability = [Product]

--a)намира името на продукта, с цена,най-близка до средната за всички продукти
remm::Double->Product->Product
remm z (x,y)= (x,y-z)

avgSum:: StoreAvailability -> Double
avgSum xs = (sum(map snd xs)) / (fromIntegral(length xs))

sortedList::StoreAvailability->StoreAvailability
sortedList xs = map (remm (avgSum xs)) xs

closestToAverage :: StoreAvailability -> String
closestToAverage xs = fst(head(sortBy (compare `on` snd) (sortedList xs) ))

-- броя на продуктите,за които има продукт със същото име, но по-ниска цена
findSame::StoreAvailability->Product->Int
findSame [] _ =0
findSame (x:xs) y
    |fst x == fst y &&snd x < snd y = 1 + findSame xs y
    |otherwise = findSame xs y

cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative xs = sum(map (findSame xs) xs)

--намира най-малкото от разстоянията между двойките точки от списъка

d::(Double,Double,Double)->(Double,Double,Double)->Double
d (x1,y1,z1) (x2,y2,z2) =  (x1-x2)*(x1-x2)+(y1-y2)*(y1-y2)+(z1-z2)*(z1-z2)

allWithOne x xs = map (d x) xs 
mapd xs = concatMap (\ x -> allWithOne x xs) xs
--mapd::[(Double,Double,Double)]->[Double]
--mapd xs = concatMap (\ x ->(map (\other ->d x other) (tail xs))) xs

minDistance :: [(Double,Double,Double)] -> Double
minDistance xs = minimum (mapd xs)

--зад 5
maximize :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maximize xs = \x-> fst (last (sortBy (compare `on` snd) [(f, fx)| f<-xs, fx<- (map (\fun ->abs(fun x)) xs)])) x

--зад 6
inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g a b
    |a>b = True
    |f a + g a - 2*a /= 0 = False
    |otherwise = inverseFun f g (a+1) b

--зад 7 Дървета
data BTree a = Empty | Node a (BTree a) (BTree a)
mirrorBst :: BTree а -> BTree а
mirrorBst Empty = Empty
mirrorBst (Node v Empty Empty) = (Node v Empty Empty)
mirrorBst (Node v left Empty ) = (Node v Empty (mirrorBst left))
mirrorBst (Node v Empty right) = (Node v (mirrorBst right) Empty)
mirrorBst (Node v left right) = (Node v (mirrorBst right) (mirrorBst left))

getLevels :: BTree a -> [(a,Int)]
getLevels t@(Node v left right) = helper t 1 where
  helper Empty _ = []
  helper (Node a l r) lvl = [(a, lvl)] ++ (helper l (lvl +1)) ++(helper r (lvl +1))
  
inorder :: BTree a -> [a]
inorder (Node v Empty Empty) = [v]
inorder t@(Node v left right) = inorder left ++ [v] ++inorder right

