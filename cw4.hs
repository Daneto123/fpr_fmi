import Data.List

main :: IO ()
main = do
 
    print(listsum [1,2,3,3,4,5,6])
    print(listsum1 [1,2,3,3,4,5,6])
    print(searchelement 4 [1,2,3,3,4,5,6] )
    print(numininterval 1 4)
    print(removeItem 1 [1,2,3,4,5,6])
    print(removeallitem 3 [1,2,3,4,4,3,3,5,6])
    print (f1 5)
    print ((\ x -> x * 2) 2)
    print ((\ x y -> x * y) 2 5)
    print (f1 2)    -- 4
    print (f3 f1 2) -- 6
    print (f3 (\ x -> x * 3) 2) -- 12
    print ((f4 2) 5) -- 20
    print (f5 5)     -- 20
    print (1 + 2)
    print ((1 +) 2)
    print (map (+ 2) [1,2,3])
    print (map ((+ 2).(* 3)) [1,2,3])
    print (incrementAllBy [1,2,3] 5)

-- task 1 tova go polzvam ot prezentaciqta
listsum :: [a] -> Int
listsum xs = length xs

-- task 2 tova sushto e ot prezentaciqta
listsum1 :: [Int] -> Int
listsum1 xs = sum xs

-- task 3 - (x:xs) za dostipfaje na list go vidqh ot internet , predpolagam ima drug narin ,no se seshtam
searchelement n [] = False
searchelement n (x:xs) 
    |n == x = True
    |otherwise = searchelement n xs 

-- task 4
numininterval :: Int -> Int -> [Int]-- ne znam dali zadachata e taka , no ne znam kak da printiram lista
numininterval a b 
    |a/=b = [] 
    |otherwise = numininterval (a+1) b

-- task 5
removeItem :: Int -> [Int] -> [Int]
removeItem n (x:xs)
    |n == x = drop n xs
    |otherwise = removeItem (n+1) xs

-- taks 6
removeallitem :: Int -> [Int] -> [Int]
removeallitem n (x:xs) = helper n xs 0
    where
     helper n xs count 
        |count == length xs = drop n xs
        |otherwise = helper n xs (count+1)

-- classwork
-- vinagi da si definirame :: Int ... i func [] = [] (krai na recursiqta)
-- Eq -> ==   ord -> <,>...

member :: Int -> [Int] -> Bool
member n [] = False
member z (x:xs) = z == x || member z xs 

removefirst :: Eq t => t -> [t] -> [t]
removefirst _ [] = []
removefirst z (x:xs) = if(z == x) then xs else x : (removefirst z xs)

-- pri spisucite te sa bezkraini strukturi kato mojem da dostupim samo purviqt element i ,ako ni vurshi rabota go zapazvame s : 
--,ako ne xs toest da produlji s ostanalite elementi

removeall :: Eq t => t -> [t] -> [t]
removeall z xs
    |null xs = []
    |z == head xs = removeall z (tail xs)
    |otherwise = (head xs) : removeall z (tail xs)
-- drug nachin na zapis e [x + z | x <- xs]

-- ideqta na map e da se priloji func na vseki element ot spisuka i zapazva duljinata na spisuka
--def na lamda func /x = x*x , moje da go napishem direktno v printa 

-- проверка дали списък е празен
null' :: [a] -> Bool
null' [] = True
null' _  = False
-- 'a' е произволен тип - може да бъде Int, Float, Char, и т.н.
-- ще го ползваме често при списъци, тъй като много от функциите няма да зависят от конкретния тип на елементите на списъка

-- взимане на глава на списък
head' :: [a] -> a
head' (x:_) = x

-- взимане на опашка на списък
tail' :: [a] -> [a]
tail' (_:xs) = xs

-- сумиране елементите на списък от цели числа
sum' :: [Int] -> Int
sum' xs = if null xs then 0 else head xs + sum' (tail xs)
-- с pattern matching
sum'' :: [Int] -> Int
sum'' []     = 0
sum'' (x:xs) = x + sum xs
-- при работа със списъци ще ползваме основно този подход - pattern matching, 
-- разглеждайки случаи за празен списък, за непразен, и други според конкретната задача

-- дължина на списък
length' :: [a] -> Integer
length' []     = 0
length' (_:xs) = 1 + length' xs

-- проверка дали число е елемент на списък
elem' :: Int -> [Int] -> Bool
elem' _ []     = False
elem' y (x:xs) = x == y || elem' y xs

-- взимане на първите n елемента на списък
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n - 1) xs

-- премахване на първите n елемента на списък
drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs

-- минимален елемент на списък от числа
minimum' :: [Int] -> Int
minimum' (x:xs) = helper xs x
    where 
        helper [] min = min
        helper (x:xs) min = if x < min then helper xs x else helper xs min

-- конкатенация на два списъка (вградената функция за това всъщност е операция: ++)

-- обръщане на списък

-- конкатениране на елементите на списък от списъци

-- classwork na grupata
f1 :: Int -> Int
f1 x = x + 2

f2 :: (Int -> Int)
f2 = \ x -> x + 2

-- f3 :: (((Int -> Int) -> Int) -> Int)
f3 :: (Int -> Int) -> Int -> Int
f3 f x = f (x * 2)

-- f4 :: Int -> Int -> Int
f4 :: Int -> (Int -> Int)
f4 x = \ z -> 2 * x * z
-- f4 x z = 2 * x * z

f5 :: Int -> Int
f5 = f4 2

-- i dvata nachina na zapis moje da se polzvat
-- map (+ 2) [1,2,3] -> [3,4,5]
-- -> ((+ 2) 1) : ((+ 2) 2) : ((+ 2) 3) : []
-- [(+ 2) x | x <- [1,2,3]]
map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 f (x:xs) = (f x) : map1 f xs

-- filter (\ x ->  mod x 2 == 1) [1,2,3] -> [1,3]
-- [x | x <- [1,2,3], mod x 2 == 1]
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ []     = []
filter1 p (x:xs) =
    if p x then x : filter1 p xs else filter1 p xs

incrementAllBy :: [Int] -> Int -> [Int]
incrementAllBy xs z = map (+ z) xs
