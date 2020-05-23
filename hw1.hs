main :: IO()
main = do

  print(findSum 0 2 10) -- → 3578 (510 + 1022 + 2046)
  print(findSum 5 3 5) -- → 174 (26 + 50 + 98)
  print(isSquare 1) -- → True
  print(isSquare 2) -- → False
  print(isSquare 4) -- → True
  print(isSquare 17) -- → False
  print(isSquare 256) -- → True
  print(isSquare 2500) -- → True 
  print(isSpecial 131 2) -- → True (числата 13 и 31 са прости) 
  print(isSpecial 472 2) -- → False (47 е просто число, но 72 не е просто) 
  print(isSpecial 17197 2) -- → True (числата 17, 71, 19 и 97 са прости) 
  print(isSpecial 12234 3) -- → False (числото 234 не е просто) 
  print(isSpecial 10113 3) -- → True (числата 101, 011 и 113 са прости) 
  print(isSpecial 353 2) -- → False (числото 35 не е просто) 

-- task 1
helper::Int -> Int -> Int -> Int -- smqta vsichki elementi
helper a b n 
  |n < 0     = a
  |otherwise = ((2^n)*b) + helper a b (n-1)

findSum :: Int -> Int -> Int -> Int -- subira samo poslednite 3 elementa
findSum a b n = helperAllSum 0 0 1
  where
    helperAllSum sum counter num
      |counter==3 = sum
      |otherwise  = helperAllSum (sum + helper a b (n-num)) (counter+1) (num+1) 

--task 2      
isSquare :: Int -> Bool 
isSquare x = helper x -- chrez recursiqta namalqvam dopulnitelniqt parametur dokato ne stane koren na x
  where
    helper n 
      |n*n > x   = helper(n-1)
      |otherwise = n*n == x 

--task 3
isPrime :: Integer -> Bool 
isPrime 1 = False
isPrime n = helper 2
  where
    helper d
      | d == n         = True
      | n `mod` d == 0 = False
      | otherwise      = helper (d + 1) 
    
isSpecial :: Integer -> Int -> Bool -- ne rabori s primera print(isSpecial 10113 3) vrushta False , trqbva da e True
isSpecial n k 
  |n == 1                         = True
  |isPrime(n `mod` 10^k) == False = False
  |otherwise                      = isSpecial (n `div` 10) k
  
