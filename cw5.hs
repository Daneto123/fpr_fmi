main :: IO()

main = do
    print(myIdentity 3)
    print(myCompose )
    --print(difference)
    --print(derive )

myIdentity :: a -> a
myIdentity x = x

myCompose ::( a -> b ) -> ( c -> a ) -> ( c -> b )
myCompose f g = \ x -> f(g x) 
-- myCompose1 f g x = f (g x) vtori nachin na zapis

myneagate :: (a -> Bool) -> (a -> Bool)
myneagate = \ x -> (not (p x)) 
-- myneagate p x = not (p x) 

mycurry :: (a -> b -> c -> d) -> a -> (b -> c -> d) 
mycurry f x = f x

difference :: (Double -> Double) -> Double -> Double -> Double
difference f a b = (f a) - (f b)

derive :: (a -> a) -> a -> (a -> a) 
derive f eps = \ x -> (f(x+eps) - f(x))/eps

secondderive :: (a -> a) -> a -> (a -> a)
secondderive f eps = derive(derive f eps) eps 

nderive :: (a -> a) -> a -> a -> (a -> a)
nderive f eps 0 = f
nderive f eps n = derive(nderive f eps (n-1)) eps

repeated :: (Double -> Double) -> Int -> (Double -> Double)
repeated f 0 = f
repeated f n = f (repeated f (n-1))  

newton-sqrt :: Int -> Int  

func :: Int -> (Int -> Int)
func n = \ x -> (x^2 - n)



 