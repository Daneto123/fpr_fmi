main :: IO()
main = do

    --task1
    print(containsWord t1 "acd") -- -> True
    print(containsWord t1 "cd") -- -> True
    print(containsWord t1 "ac") -- -> False
    --task2
    print(genWords t1) -- -> ["acf","acd","abe","cf","cd","f","d","be","e"]
    --task3
    print(allContain [t1, t2]) -- -> ["acd","cd","d"]
    print(unique (allContain [t1, t2]))
    --task4
    --print(isGraceful t3) -- -> True (|3-1|=2, |5-1|=4, |7-1|=6, |9-1|=8)
    --print(isGraceful t4) -- -> False (|9-7|=2, |5-9|=4, |2-9|=7)

data BTree a = Empty | Node a (BTree a) (BTree a)

t1 :: BTree Char                                 --   a
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)   --  / \
                        (Node 'd' Empty Empty))  --  c  b
                        (Node 'b' Empty          -- / \  \
                (Node 'e' Empty Empty))          -- f  d  e 

t2 :: BTree Char                                 --   a
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty)   --  / \
                                        Empty)   --  c  b
                         (Node 'b' Empty Empty)  -- /
                                                 -- d 

--task1
containsWord :: BTree Char -> String -> Bool
containsWord Empty [] = True
containsWord tree  [] = True
containsWord Empty word = False
containsWord tree@(Node char lt rt) word@(x:xs)
    | (char == x) && (search (treeLeaves tree) word)= ((containsWord lt xs) || (containsWord rt xs))
    | otherwise = ((containsWord lt [x]) || (containsWord rt [x]))

treeLeaves :: BTree Char -> String
treeLeaves Empty                   = []
treeLeaves (Node char Empty Empty) = char:[]
treeLeaves (Node _ lt rt)          = treeLeaves lt ++ treeLeaves rt

search :: String -> String -> Bool
search [] _ = False
search (x:xs) ys
    | (x == (last ys)) = True
    | otherwise = search xs ys

--task2
genWords :: BTree Char -> [String] -- използвам map с con ,за да получа всички думи от дървото
genWords Empty = []
genWords (Node char Empty Empty) = [[char]]
genWords (Node char lt rt) = map (char:) helper ++ helper
    where
        helper = genWords lt ++ genWords rt
        
--task3
allContain :: [BTree Char] -> [String] -- чрез list comprehension , за да напълня всички думи в list
allContain [] = []
allContain (x:xs) = [y | x <- (x:xs), y <- genWords x]

isIn :: String -> [String] -> Bool -- проверява дали думата я има
isIn _ [] = False
isIn n (x:xs)
    | n == x = True
    | otherwise = isIn n xs

unique :: [String] -> [String] -- ако я има я записва в нов list
unique [] = []
unique (x:xs)
    | (isIn x xs) == True = [x] ++ unique xs
    | otherwise = unique xs

data NTree = Nil | NNode Int [NTree]

t3 :: NTree               --    1
t3 = NNode 1 [NNode 3 [], -- / / \ \
              NNode 5 [], -- 3 5  7 9
              NNode 7 [],
              NNode 9 []]

                                      --  7
t4 :: NTree                           --  |
t4 = NNode 7 [NNode 9 [NNode 5 [],    --  9
 NNode 2 []]]                         -- / \
                                      -- 5  2

--task4
-- isGraceful :: NTree -> Bool
-- isGraceful (NNode num []) = True
-- isGraceful (NNode num ((NNode num1 (y:ys)):xs)) = ((num - num1) `mod` 2 == 0) && isGraceful (NNode num1 xs)

