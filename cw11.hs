-- Задача 1. Да се дефинират рекурсивен алгебричен тип двоично дърво (Tree) и следните функции:
-- a) функция size, която намира броя на елементите на двоично дърво
-- b) функция height, която намира височината на двоично дърво
-- c) функция sumTree, която намира сумата от елементите на двоично дърво
-- d) функция sumLeaves, която намира сумата елементите по листата на двоично дърво
-- e) функция inorder, която обхожда двоично дърво в ред Ляво-Корен-Дясно

-- Примери:

t1 :: Tree                                          --    5
t1 = Node 5 (Node 2 Empty                           --   / \
                (Node 3 Empty Empty))               --  2   6
                    (Node 6 Empty Empty)            --   \
                                                    --    3                                         

t2 :: Tree                                         --    5
t2 = Node 5 (Node 3 Empty Empty)                   --   / \
                   (Node 4 (Node 5 Empty Empty)    --  3   4
                          (Node 7 Empty Empty))    --     / \
                                                   --    5   7

-- Задача 2. Да се дефинира функция getLevel :: getLevel :: Int -> Tree -> [Int], която намира елементите на k-то ниво на двоично дърво.
-- Задача 3. Да се дефинира функция average :: Tree -> Double, която пресмята средно-аритметичното от записаното във върховете на двоично дърво.
-- Задача 4. Да се дефинира функция mirrortree :: Tree -> Tree, която преобразува дърво в "огледалното" му.

-- Пример:  

t3 :: Tree                                                      --       1                1
t3 = Node 1 (Node 2 (Node 5 Empty Empty) Empty)                 --      / \              / \
                    (Node 3 (Node 7 Empty Empty)                --    2    3   =>      3    2
                        (Node 6 Empty Empty))                   --   /    / \         / \    \
                                                                --  5    7   6       6   7    5

main :: IO()
main = do

    -- task1 а)
    print(size t1)
    --task1 б)
    --print(height t1)
    --task1 в)
    print(sumtree t1)
    --task1 г)
    print(sumleaves t1)
    --task2
    print(getLevel 1 t2)
    print(getLevel 2 t2)
    print(getLevel 3 t2)
    --task3
    print(average t1)
    print(average t2)
    print(average t3)
    --task4
    print(mirrortree t1)
    print(mirrortree t2)
    print(mirrortree t3)

data Tree = Empty | Node Int Tree Tree
    deriving Show

--task1 а)
size :: Tree -> Int -- с lt е означено дали дървото има ляв наследник
size Empty          = 0
size (Node _ lt rt) = 1 + size lt + size rt

--task1 б)
-- height :: Tree -> Int
-- height Empty          = 0
-- height (Node _ lt rt) = 1 + max (height lt) + (height rt)

--task1 в)
sumtree :: Tree -> Int
sumtree Empty            = 0
sumtree (Node num lt rt) = num + (sumtree lt) + (sumtree rt)

--task1 г)
sumleaves :: Tree -> Int
sumleaves Empty                  = 0
sumleaves (Node num Empty Empty) = num
sumleaves (Node _ lt rt)         = sumleaves lt + sumleaves rt

--task2
getLevel :: Int -> Tree -> [Int]
getLevel _ Empty          = []
getLevel 1 (Node num _ _) = [num]
getLevel k (Node _ lt rt) = (getLevel (k - 1) lt) ++ (getLevel (k - 1) rt)

--task3
average :: Tree -> Double
average tree = fromIntegral (sumtree tree) / fromIntegral (size tree)

--task4
mirrortree :: Tree -> Tree
mirrortree Empty = Empty
mirrortree (Node num lt rt) = Node num (mirrortree rt) (mirrortree lt)


