import Data.Char

main :: IO()
main = do
    
    --task1
    print(generate 1 3) -- [1.0,1.5,1.8333333333333333]
    print(generate 0.1 5) -- [1.0,1.93,2.83,3.70,4.55]
    --task2
    print(listSquares 1 30) -- [(1,1),(4,21),(9,91),(16,341),(25,651)]
    print(listSquares 250 300) -- [(256,87381),(289,83811)]
    --task3
    print(splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) -- ([(1.0,2.0),(2.0,3.0),(-1.0,1.0)],[(10.0,15.0),(12.0,14.0)])
    print(splitPoints (10,10) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]) -- ([(10.0,15.0),(12.0,14.0)],[(1.0,2.0),(2.0,3.0),(-1.0,1.0)])

--task 1
helper :: Double -> Double -> Double -> Double -> Double -- dopulnitelna funciq da smqta sumata
helper sum p n step
    | step == n+1 = sum 
    | otherwise   = helper (sum + (1 / (step**p))) p n (step + 1)

generate :: Double -> Double -> [Double]
generate p n = helper1 1 p n
    where
        helper1 counter p n
            | counter == n = [(helper 0 p counter 1)]
            | otherwise    = [(helper 0 p counter 1)] ++ helper1 (counter+1) p n

--task 2
sumDivisors :: Int -> Int --subira vsichki deliteli na chisloto
sumDivisors n = helper 1 0
  where
    helper d sum
      | d == n          = sum + d^2
      | n `mod` d == 0  = helper (d + 1) (sum + d^2)
      | otherwise       = helper (d + 1) sum

isSquare :: Int -> Bool -- vzel sum q ot minaloto domashno da proverqva za tochen kvadrat
isSquare x = helper x
    where
        helper n 
            |n*n > x   = helper(n-1)
            |otherwise = n*n == x 

listSquares :: Int -> Int -> [(Int,Int)]
listSquares 0 0 = []
listSquares a b = helper a b b
    where
        helper _ _ 0 = []
        helper a b count
            | isSquare(a) == True && (a < b) = [(a, sumDivisors (a))] ++ helper (a + 1) b (count - 1)
            | otherwise                      = helper (a + 1) b (count - 1)

--task 3
type Point = (Double, Double)

splitPointsIn :: Point -> Double -> [Point] -> [Point] --smqta tochkite koito sa v kruga
splitPointsIn _ _ [] = []
splitPointsIn (xc, yc) r ((x, y):xs)
    | sqrt((xc-x)^2 + (yc-y)^2) <= r = [(x, y)] ++ splitPointsIn (xc, yc) r xs
    | otherwise                      = splitPointsIn (xc, yc) r xs

splitPointsout :: Point -> Double -> [Point] -> [Point] --smqta tochkite izvun kruga 
splitPointsout _ _ [] = []
splitPointsout (xc, yc) r ((x, y):xs)
    | sqrt((xc-x)^2 + (yc-y)^2) > r = [(x, y)] ++ splitPointsout (xc, yc) r xs
    | otherwise                     = splitPointsout (xc, yc) r xs

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point]) --obedinqva dvete funcii v edin spisuk
splitPoints (xc, yc) r ((x, y):xs) = (splitPointsIn (xc, yc) r ((x, y):xs), splitPointsout (xc, yc) r ((x, y):xs))

--task 4
-- type Account = (Int, Int, Double)
-- type Person  = (Int, String, String)

-- ps :: [Person]
-- ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]

-- as :: [Account]
-- as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

-- getName :: Person -> String
-- getName (_, name, _) = name

-- getCity :: Person -> String
-- getCity (_, _, city) = city

-- getValue :: Account -> Double
-- getValue (_, _, value) = value

-- getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
-- getAverageBalance (as,ps) person = 
    