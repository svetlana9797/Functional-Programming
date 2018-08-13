--problem 3
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)

--returns the height of the tree
height::Tree a->Int
height Empty = 0
height (Node a left right)
    |hl >hr = 1 + hl
    |otherwise = 1+hr
    where hl = height left; hr = height right

--returns the difference of the heights of the left and right subtree
difference::Tree a ->Int
difference Empty = 0
difference t@(Node a left right) = abs (height left - height right)

isBalanced::Tree a -> Int -> Bool
isBalanced Empty _ = True
isBalanced tree@(Node a left right) k = (difference tree<= k)&&(isBalanced left k) && (isBalanced right k)
