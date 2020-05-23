-- Задача 1. Да се дефинира функция primesInRange :: Integer -> Integer -> [Integer], която конструира списък от простите числа в интервала [a,b].
-- Задача 2. Да се дефинира функция prodSumDiv :: [Integer] -> Integer -> Integer, която намира произведението на естествените числа в даден списък,
-- сумата от делителите на които е кратна на k.
-- Задача 3. Да се дефинира функция isSorted :: [Int] -> Bool, която проверява дали списък е сортиран във възходящ ред.
-- Задача 4. Да се дефинира функция insert :: Int -> [Int] -> [Int], която добавя елемент в сортиран списък, като резултатният списък също е сортиран.
-- Задача 5. Да се дефинира функция merge :: [Int] -> [Int] -> [Int], която получава два сортирани списъка и ги обединява така, че резултатът също да е сортиран.
-- Задача 6. Да се реализира функция insertionSort :: [Int] -> [Int], която реализира сортиране чрез вмъкване върху списък.
main :: IO()
main = do
    print(primesInRange 5 10)
    print(prodSumDiv [1,2,3,4,5] 4)
    print(isSorted [3,4,5,6,7,8,9,10])
    print(insert 5 [3,4,7,8,9,10])
    print(merge [1,3,5,7,9] [4,6,8,10])
    -- print()

--task1
primesInRange :: Integer -> Integer -> [Integer]
primesInRange a b = [k | k <- [a..b] , [1, k] == [d |d <- [1..k] , k `mod` d == 0]]

--task2
prodSumDiv :: [Integer] -> Integer -> Integer
prodSumDiv xs k = product [x | x <- xs, sum [d | d <- [1..x], x `mod` d == 0] `mod` k == 0]

--task3
isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [_] = True 
isSorted (x:y:xs) = x<=y && isSorted(y:xs)

--task4
insert :: Int -> [Int] -> [Int]
insert _ [] = []
insert n (x:xs)
    | n <= x = (n:x:xs)
    | otherwise = x : insert n xs

--task5
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge zs@(x:xs) ts@(y:ys)
    | x <= y = x : merge xs ts
    | otherwise = y : merge zs ys

--task6
--insertionSort :: [Int] -> [Int]
--insertionSort (x:xs)
    