-- Задача 1. Да се дефинира функция multiplyAllBy :: [Int] -> Int -> [Int], която получава списък и число и умножава всеки елемент на списъка по числото.
-- Задача 2. Да се дефинира filterSmallerThan xs n, която получава списък xs и число n и премахва елементите на списъка xs, които са по-малко от числото n.
-- Задача 3. Да се дефинира функция isAscending :: Integer -> Bool, която проверява дали цифрите на число са във възходящ ред. Функцията да 
-- получава число, но да работи със списък от цифрите му.
-- Задача 4. Нека as = [a1, a2, … , ak] и bs = [b1, b2, … , bk] са непразни списъци с еднакъв брой числа. Да се дефинира предикат 
-- isImage :: [Int] -> [Int] -> Bool, който да връща „истина“ точно когато съществува такова число x, че ai = x + bi за всяко i = 1,..., k.
-- Задача 5. Да се дефинира функция chunksOf :: Int -> [a] -> [[a]], която разделя входния списък на подсписъци с дължина равна на подаденото число.
-- Задача 6. Да се дефинира функция divisors :: Integer -> [Integer], която генерира списък от всички (собствени) делители на дадено число.
-- Задача 7. Да се дефинира предикат isTriangular :: [[Int]] -> Bool, който получава квадратна числова матрица, представена като списък от списъци,
--  и проверява дали тя е горно триъгълна, т.е. дали всичките елементи под главния ѝ диагонал са нули.
import Data.Char

main :: IO()
main = do
    print(multiplyAllBy [1,2,3,4] 3)
    print(filterSmallerThan [1,2,3,4,5] 3)
    print(isAscending 1234)
    print(isImage [1,2,3] [6,7,8])
    print(chunksOf 3 [1,2,3,4])
    print(divisors 49)
    print(ord 'a') -- 97 ascii code
    print(chr 97) -- a
    print(ord '5' - ord '0') -- 5
    print(show 12345) -- '12345'
    print(tolist 12345) -- '12345'


--task1
multiplyAllBy :: [Int] -> Int -> [Int]
multiplyAllBy xs n = [x*n | x <- xs]

--task2
filterSmallerThan :: [Int] -> Int -> [Int]
filterSmallerThan xs n = [x | x <- xs , x >= n]

--task3
isAscending :: Int -> Bool
isAscending n = check (reverse (convert n))

convert :: Int -> [Int]
convert x
    |x < 10 = [x]
    |otherwise = (x `mod` 10) : convert (x `div` 10) 

check :: [Int] -> Bool
check [] = True
check [_] = True
check (x:y:xs) = x<y && check(y:xs)

--task4
isImage :: [Int] -> [Int] -> Bool
isImage (a:as) (b:bs) = helper as bs (b - a)
    where
        helper []     []     _ = True
        helper (a:as) (b:bs) x = (b - a) == x && helper as bs x
        

--task5
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- --task6
divisors :: Int -> [Int]
-- divisors x = helper 2 k
--     where
--      helper n k
--         |n * n > k = [k]
--         |n * n == k = [n,k]
--         |(k `mod` n) == 0 = (n : helper(k `div` n))
--         |otherwise = helper (n+1) k
divisors n = [d | d <- [1..n-1] , (n `mod` d) == 0]

--more task
tolist :: Integer -> [Int]
tolist n = [ord d - ord '0' | d <- show n]

checks :: [Int] -> Bool 
checkss ds = and [a < b | (a,b) <- zip ds (tail ds)] -- codene na zip
