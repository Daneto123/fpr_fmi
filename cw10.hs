-- Задача 1. Да се дефинира тип Shape с 4 конструктора: 
-- Circle, който има 1 аргумент  - радиус
-- Rectangle, който има 2 аргумента - ширина и височина
-- Triangle, който има 3 аргумента - 3 страни
-- Cylinder, който има 2 аргумента - радиус на основата и височина
-- Нека Shape да е екземпляр на класа Show и за него да се дефинира метода show.

-- Задача 2. За Shape да се дефинират:
-- a) функция perimeter :: Shape -> Double, която намира периметъра на фигурата
-- b) функция area :: Shape -> Double, която намира лицето на фигурата
-- c) предикат isRound :: Shape -> Bool, който проверява дали дадена фигура е кръгла (някоя от стените е окръжност)
-- d) предикат is2D :: Shape -> Bool, който проверява дали дадена фигура е равнинна (лежи в една равнина)

-- Задача 3. Да се дефинира функция sumArea, която приема списък от фигури и връща сумата от лицата на фигурите в списъка. 
-- Да се дефинира още една функция biggestShape, която намира фигурата с най-голямо лице.

-- Задача 4. Да се дефинира тип Point, който задава точка в равнината и точка в пространството. 
-- Нека да е екземпляр на класа Eq и за него да се дефинира равенство на точки от една и съща размерност.

-- Задача 5. Да се дефинира функция distance за работа с типа Point, която намира разстоянието между две (съвместими) точки. 
-- Ако точките са с различна размерност (т.е имат различен брой координати) функцията да връща съобщение за грешка.

-- Задача 6. Да се дефинира функция getClosestPoint, която приема списък от точки и още една точка p. 
-- Като резултат функцията да връща тази точка от списъка, която е най-близо до точката p.

-- Задача 7. Да се дефинира функция getClosestDistance :: [Point] -> (Double, Point, Point), която приема списък ps от точки от тип Point. 
-- Като резултат функцията да връща тройка съставена от най-близкото разстояние между две точки от списъка ps, както и конкретните точки, намиращи се на това разстояние.
-- Заб.: Ако две точки са с различна размерност, то разстоянието между тях се счита за безкрайно голямо.
import Data.List

main :: IO()
main = do

--task1
    print 10
    print (s1)
    print (isWinter s1)
    print (isWinter s2)
    print (perimeter (Rectangle 10 15))
    print (perimeter (Triangle 2 3 4))
--task2
    print(isRound (Circle 10))
    print(isRound (Cylinder 10 16))
    print(isRound (Triangle 10 10 10))
    print(is2D (Circle 10))
    print(is2D (Cylinder 10 16))
    print(is2D (Triangle 10 10 10))
    --task3
    print(sumArea [(Circle 10), (Triangle 10 10 16), (Rectangle 10 16)])
    --task6
    print (getClosestPoint ps (Point2D 0 0))
    --task7
    print (getClosestDistance ps)    

fs :: [Shape]
fs = [(Rectangle 2 5), (Rectangle 3 7), (Circle 1)]

ps :: [Point]
ps = [(Point2D 1 2), (Point2D 2 3), (Point2D 1 0), (Point3D 1 0 1)]

{-

1, 2, 3, ...
1.2, 2.2, ...
'c'
"abc"
[1, 2, 3]
[[1], [2, 3], [3, 4, 5]]
(1, 'a')
(1, 1.2, 'b')

-}

--task1
data Season = Spring | Summer | Autumn | Winter
    deriving Show

s1 :: Season
s1 = Winter

s2 :: Season
s2 = Autumn

isWinter :: Season -> Bool
isWinter Winter = True
isWinter _      = False

-- това е структура с нея могат да се правят всички неща коѝто се првят и с лист
data Shape = Circle Double |
             Rectangle Double Double |
             Triangle Double Double Double |
             Cylinder Double Double
             deriving Show 
-- deriving Show трябват за да могат да се достъпят обектите

--task2 a)
perimeter :: Shape -> Double
perimeter (Circle r) = 3.14 * 2 * r
perimeter (Rectangle a b) = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c
perimeter (Cylinder r h) = 2 * (3.14 * r * r) + h * (3.14 * 2 * r)

--task2 b) cilidura ne e napraven
area :: Shape -> Double
area (Circle radius)     = pi * radius * radius
area (Rectangle a b)     = a * b

--task2 c)
isRound :: Shape -> Bool
isRound (Circle _)     = True
isRound (Cylinder _ _) = True
isRound _              = False

--task2 d)
is2D :: Shape -> Bool
is2D (Cylinder _ _) = False
is2D _              = True

--task3
sumArea :: [Shape] -> Double
sumArea shapes = foldr1 (+) (map area shapes)

--task4
biggestArea :: [Shape] -> Double
biggestArea fs = maximum [area f | f <- fs]

data Point = Point2D Double Double | Point3D Double Double Double deriving (Eq, Ord)

instance Show Point where
    show (Point2D a b)   = "(" ++ show a ++ ", " ++ show b ++ ")"
    show (Point3D a b c) = "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"
    --show (Point3D a b c) = show (a, b, c)

--task5
distance :: Point -> Point -> Double
distance (Point2D _ _)      (Point3D _ _ _)    = error "not compatible"
distance (Point3D _ _ _)    (Point2D _ _)      = error "not compatible"
distance (Point2D a1 b1)    (Point2D a2 b2)    = sqrt ((a1 - a2) ** 2 + (b1 - b2) ** 2)
distance (Point3D a1 b1 c1) (Point3D a2 b2 c2) = sqrt ((a1 - a2) ** 2 + (b1 - b2) ** 2 + (c1 - c2) ** 2)

isCompatible :: Point -> Point -> Bool
isCompatible (Point2D _ _)   (Point3D _ _ _) = False
isCompatible (Point3D _ _ _) (Point2D _ _)   = False
isCompatible _             _                 = True

--task6
getClosestPoint :: [Point] -> Point -> Point
getClosestPoint ps p = snd (minimum [(distance p k, k) | k <- ps, isCompatible p k]) -- snd връща вторият елемент от наредена двоѝка числа

--task7
getClosestDistance :: [Point] -> (Double, Point, Point)
getClosestDistance ps = minimum [(distance p q, p, q) | p <- ps, q <- delete p ps, isCompatible p q] -- delete премахва първият елемент от даден лист