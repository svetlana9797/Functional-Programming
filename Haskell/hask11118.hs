--import Prelude hiding (foldl, foldr, minimum, maximum, sum, product, length, and, or, any, all, concat, reverse)
--sum xs, която връща сбора на елементите на xs
sum':: (Num t)=>[t]->t
sum' xs = foldl(\ acc x-> acc + x) 0 xs

sum'' xs = foldl1 (+) xs

--product xs, която връща произведението на елементите на xs
product'::(Num t)=>[t]->t
product' xs = foldl (\acc x -> acc*x) 1 xs

--length xs, която връща броя на елементите на xs
length' :: [t]->Int
length' xs = foldl (\acc x ->acc + 1) 0 xs

-- дали предикатът p e верен за поне един от елементите на xs
any' :: (t->Bool)->[t]->Bool
any' p xs = foldr (||) False (map p xs)

--дали предикатът p e верен за всеки от елементите на xs
all' :: (t->Bool)->[t]->Bool
all' p xs = foldr (&&) True (map p xs)

--връща най-малкия елемент на xs
minimum' ::(Ord t)=>[t]->t
minimum' xs = foldl1 (min) xs

-- връща най-големия елемент на xs
maximum' :: (Ord t)=>[t]->t
maximum' xs = foldr1 max xs

--приема списък от списъци xss и ги конкатенира в един общ
concat'::[[t]] -> [t]
concat' xss = foldr1 (++) xss

--приема списък xs и обръща елементите му
reverse':: [t]->[t]
reverse' xs = foldl (\ acc x -> x:acc) [] xs

--приема списък от едноаргументни функции и връща тяхната композиция
compose' :: [(t->t)]->(t->t)
compose' fs x = foldl (.) id fs x
