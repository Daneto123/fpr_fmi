-- Задача 1. Двоичното дърво, което използвахме досега винаги имаше Int във възлите си. Да се дефинира нов алгебричен тип BTree a, за който може да се определя и типа на възлите. За него да се:
-- a) преработи функцията sumTree от миналия път, като сега тя трябва да работи само за дървета, чиито възли настина могат да бъдат сумирани;
-- b) дефинира равенство на дървета.

-- Задача 2. Да се дефинира функцията getLevelsTree :: BTree a -> BTree (a,Int), която заменя всеки възел с двойка от стойността на възела и номера на нивото му 
-- Примерно дърво:
t1 :: BTree Int                           --     1                  (1,0)
t1 = Node 1 (Node 2 (Node 5 Empty Empty)  --    / \                 /    \
                                   Empty) --   2   3   =>         (2,1)  (3,1)
            (Node 3 (Node 7 Empty Empty)  --  /   / \            /       /  \
                    (Node 6 Empty Empty)) -- 5   7   6         (5,2)  (7,2)  (6,2)

-- Задача 3. Да се дефинира функция mapTree :: (a -> b) -> BTree a -> BTree b, която прилага дадена функция f към всеки възел на дървото.

-- Задача 4. Нека имаме типът за цвят data Color = Red | Green | Blue deriving (Read, Show, Eq)
-- Дефинирайте функция maxDepthBlueNode :: BTree Color -> Int, която намира дълбочината на най-дълбокия (най-отдалечения от корена) връх с цвят Blue на дадено двоично дърво от тип Color.

-- Пример:
colorTree :: BTree Color                                              --           Blue
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty)       --          /    \
                        (Node Red (Node Blue (Node Green Empty Empty) --        Red    Red
                                           (Node Red Empty Empty))    --        /      /  
                                    Empty)                            --       Green  Blue  
                                                                      --         /     \
                                                                      --       Green   Red

-- Задача 5. Дефинирайте функция maxDepthNode :: BTree Color -> Color -> Int, която намира дълбочината на най-дълбокия връх с цвят, подаден като аргумент на дадено двоично дърво от тип Color. 

-- Задача 6. Да се дефинира алгебричен тип NTree а, който да представлява дърво с произволен брой наследника на всеки възел. За него да се дефинира фунцкия size, която брои елементите му

-- Примерно дърво:
t4 :: NTree Int                               --       1
t4 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --      / \
                        (NNode 4 [NEmpty]),   --     2   6
                        (NNode 5 [NEmpty])]), --    /|\  |
              (NNode 6 [(NNode 7 [NEmpty])])] --   3 4 5 7

main :: IO()
main =  do
    --task1
    --print()
    --task2
    print(getLevelsTree t1)
    --task3
    print(mapTree t1 (\x->x+1))
    --task4
    print(maxDepthBlueNode colorTree)
    --task5
    --print()

--task1

--task2
getLevelsTree :: BTree a -> BTree (a, Int)
getLevelsTree Empty = Empty
getLevelsTree tree  = helper tree 0
    where
        helper Empty _              = Empty
        helper (Node a lt rt) level = (Node (a, level) (helper lt (level + 1)) (helper rt (level + 1)))

--task3
mapTree :: (a -> b) -> BTree a -> BTree b
mapTree f Empty          = Empty
mapTree f (Node v lt rt) = (Node (f v) mapTree lt mapTree rt)

--task4
data Color = Red | Green | Blue deriving (Read, Show, Eq)

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode Empty = 0
maxDepthBlueNode tree = helper tree 0
    where
        helper (Node color lt rt) level
            | color == "Blue" = level
            | otherwise = (Node color (helper lt (level + 1)) (helper rt (level + 1)))

