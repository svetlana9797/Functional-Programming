--Дефинирайте следните основни процедури за работа с двоични дървета:

data Btree a = Empty | Node a (Btree a) (Btree a) deriving (Read, Show)

--(empty? tree) - проверява дали двоичното дърво tree е празно
isEmpty Empty = True
isEmpty _ = False


--дълбочина на дърво
depth_tree Empty = 0
depth_tree (Node a left right) = 1 + max(depth_tree left) (depth_tree right)

--(root tree) - връща корена на двоичното дърво tree.
root Empty = 0
root (Node a left right) = a

--(left tree) - връща лявото поддърво на двоичното дърво tree
left_tree Empty = Empty
left_tree (Node a left right) = left

--(right tree) - връща дясното поддърво на двоичното дърво tree.
rigt_tree Empty = Empty
right_tree (Node a left right) = right

--(leaf? tree) - проверява дали tree е листо.
leaf Empty = False
leaf (Node _ Empty Empty) = True
leaf _ = False

--Дефинирайте процедури (pre-order tree), (in-order tree) и (post-order tree), които правят съответно корен-ляво-дясно, ляво-корен-дясно и ляво-дясно-корен обхождане на двоичното дървото tree. 
--Процедурите да връщат списък от върховете на tree в реда на тяхното обхождане.
preorder Empty = []
preorder (Node a left right) = [a] ++ preorder left ++preorder right

inorder Empty = []
inorder (Node a left right) = inorder left ++ [a] ++ inorder right

postorder Empty = []
postorder (Node a left right) = postorder left ++ postorder right ++ [a]

--Дефинирайте процедура (level n tree), която връща n-тото ниво в двоичното дърво tree.
level _ Empty = []
level 0 (Node a left right) = [a]
level n (Node a left right) = level (n-1) left ++ level (n-1) right

--Дефинирайте процедура (count-leaves tree), която намира броя на листата в двоичното дърво tree.
countleaves Empty = 0
countleaves (Node a Empty Empty) = 1
countleaves (Node a left right) = countleaves left + countleaves right

--Дефинирайте процедура (map-tree fn tree), която прилага процедурата fn върху всеки връх на двоичното дърво tree.
maptree _ Empty = Empty
maptree f (Node a Empty Empty) = Node (f a) Empty Empty
maptree f (Node a left right) = Node (f a) (maptree f left) (maptree f right)

--Дефинирайте функция maxSumPath, която приема за аргумент двоично дърво t с числа във възлите и намира максималната сума на числата по някой път от корен до листо
maxSumPath Empty = 0
maxSumPath (Node a left right) = a  + max (maxSumPath left) (maxSumPath right)

--Дефинирайте функция bloom, която по дадено двоично дърво t връща ново дърво t', което представлява t,
--в което на всички листа са добавени по два наследника - нови листа със същата стойност като на родителите си.
bloom Empty = Empty
bloom (Node a Empty Empty) = Node a (Node a Empty Empty) (Node a Empty Empty)
bloom (Node a left right) = Node a (bloom left) (bloom right)

--Дефинирайте функция prune, която по дадено двоично дърво t връща ново дърво t',
-- което представлява t, в което всички листа са премахнати
prune Empty = Empty
prune (Node a Empty Empty) = Empty
prune (Node a left right) = Node a (prune left) (prune right)

--Extract all elements from a tree to a list
treetolist Empty = []
treetolist (Node a left right) = (treetolist left) ++ [a] ++ (treetolist right)

--Mirror a tree
mirrorBst :: BTree а -> BTree а
mirrorBst Empty = Empty
mirrorBst (Node v Empty Empty) = (Node v Empty Empty)
mirrorBst (Node v left Empty ) = (Node v Empty (mirrorBst left))
mirrorBst (Node v Empty right) = (Node v (mirrorBst right) Empty)
mirrorBst (Node v left right) = (Node v (mirrorBst right) (mirrorBst left))


--Дефинирайте тип BST, който да представлява двоично наредено дърво, съдържащо стойности от произволен тип във възлите си.
data BST a = Et| Node a (BST a) (BST a)
tree1  = (Node 4 
              (Node 2 
                    (Node 1 Empty Empty)
                    (Node 3 Empty Empty))
                (Node 5 
                    Empty
                    (Node 6 
					    (Node 7 Empty Empty)  
						Empty)))

tree2 = (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty))(Node 7 Empty Empty))

tree3 = (Node 1 (Node 2 (Node 3 Empty Empty) (Node 4 Empty Empty)) Empty)