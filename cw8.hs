-- Задача 1. Нормализация на входните данни
-- Енигма, както повечето криптиращи машини от това време, е разполагала с клавиатура със само 26-те главни букви от латинската азбука. Затова, преди да бъдат криптирани, всички съобщения трябвало да бъдат приведени в т. нар. нормален вид: всички числени стойности бивали изписвани словом, всички малки букви ставали главни, а интервалите и пунктуационните знакове били премахвани или заменяни с кодови комбинации от главни букви (напр. интервалът бил заменян с X и т. н.)
-- Дефинирайте функция normalize message, която нормализира входното съобщение.
-- Правилата за нормализация са следните:
-- - Всички малки букви стават главни. 
-- - Ако съобщението съдържа цифри, функцията връща грешка. 
-- - Всички останали символи се игнорират.
-- Примери:
-- normalize "Attack London tomorrow at ten a.m." = "ATTACKLONDONTOMORROWATTENAM"
-- normalize "Attack London tomorrow at 10 a.m." = error “digits not allowed”

-- Задача 2. Цезаров шифър
-- Цезаровият шифър е един от най-простите и най-стари методи за криптиране на съобщения. Първото му известно използване е от Юлий Цезар по време на кампаниите му в Галия, откъдето идва и неговото име. Идеята на Цезаровия шифър е проста: вземаме съобщението, което искаме да шифроваме, и заместваме всяка от буквите в него с буквата, отместена с определен брой позиции в азбуката. Например, ако отместването е 3, то тогава ‘A’ -> ‘D’, ‘B’ -> ‘E’, ‘C’ -> ‘F,’ … , ‘X’ -> ‘A’, ‘Y’ -> ‘B’, ‘Z’ -> ‘C’.
-- а) дефинирайте функция encode alphabet ch offset, която приема списък от знакове alphabet, знак ch и отместване offset и връща знака от alphabet, отместен на offset от ch (по модул дължината на списъкa). Функцията encode трябва да работи както с положително, така и с отрицателно отместване и да връща грешка, ако ch не е елемент на alphabet. N.B. Не е задължително буквите в alphabet да са подредени от ‘A’ до ‘Z’, т.е. НЕ може да разчитате на функциите ord и chr!
-- Примери:
-- encode ['A'..'Z'] 'A' 1 = 'B'
-- encode ['A'..'Z'] 'C' 2 = 'E'
-- encode ['A'..'Z'] 'Z' 3 = 'C'
-- encode ['A'..'Z'] 'A' (-1) = 'Z'
-- encode ['A'..'Z'] 'C' (-2) = 'A'
-- encode ['A'..'Z'] 'Z' (-3) = 'W'
-- encode ['A'..'Z'] '@' 1 = error “unsupported symbol: @”
-- б) дефинирайте функция encrypt alphabet offset normalized, която приема азбука alphabet, отместване offset и съобщение в нормализиран вид и връща съобщението, криптирано със съответното отместване.
-- Пример: encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM" = "FYYFHPQTSITSYTRTWWTBFYYJSFR" 
-- в) дефинирайте функция decrypt alphabet offset encrypted, която приема отместване offset и съобщение, криптирано с това отместване, и връща оригиналното съобщение в нормализиран вид. Можете да използвате факта, че декриптирането на Цезаров шифър с дадено отместване offset е еквивалентно на криптиране с отместване -offset.
-- Пример:
-- decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR" = "ATTACKLONDONTOMORROWATTENAM"
import Data.Char

main :: IO()
main = do
    print(normalize "Attack London tomorrow at ten a.m.")
    print(normalize "Attack London tomorrow at 10 a.m.")
    print(encode ['A'..'Z'] 'A' 1)-- = 'B'
    print(encode ['A'..'Z'] 'C' 2)-- = 'E'
    print(encode ['A'..'Z'] 'Z' 3)-- = 'C'
    print(encode ['A'..'Z'] 'A' (-1))-- = 'Z'
    print(encode ['A'..'Z'] 'C' (-2))-- = 'A'
    print(encode ['A'..'Z'] 'Z' (-3))-- = 'W'
    print(encode ['A'..'Z'] '@' 1)-- = error “unsupported symbol: @”
    --print(encrypt ['A'..'Z'] 5 "ATTACKLONDONTOMORROWATTENAM")-- = "FYYFHPQTSITSYTRTWWTBFYYJSFR" 
    --print(decrypt ['A'..'Z'] 5 "FYYFHPQTSITSYTRTWWTBFYYJSFR"))-- = "ATTACKLONDONTOMORROWATTENAM"


--task1
normalize :: String -> String
normalize "" = ""
normalize (x:xs) 
    | x >= '0' && x <= '9' = error "not digit allowed" 
    | x >= 'A' && x <= 'Z' = x : normalize xs
    | x > 'a' && x < 'z'   = chr (ord x - ord 'a' + ord 'A') : normalize xs 
    | otherwise = normalize xs

--task2
encode :: [Char] -> Char -> Int -> Char
encode alphabet ch offset = helper alphabet ch offset
    where
        findSymbol [] = error "unsupported symbol"
        findSymbol alphabet@(a:as) ch = 
            if a == ch then alphabet else findSymbol as ch

        offsetSymbol [] offset = offsetSymbol alphabet offset
        offsetSymbol (a:_) 0 = a
        offsetSymbol (_:as) offset = offsetSymbol as (offset- 1)
    
encrypt :: [Char] -> Int -> String -> String
encrypt alphabet ch offset normalized  = [encode alphabet ch offset]

encrypt1 :: [Char] -> Int -> String -> String
encrypt1 alphabet offset normalized = map(\ch -> encode alphabet ch offset) normalized

crackall :: [Char] -> String -> [[Char]]
crackall_alphabet_encrypted alphabet = [decrypt alphabet o encrypted | o<-[1 .. 25]]

--task na 3-ta grupa
type Product = (String, Int, Double)
type Shop = [Product]

getPrice :: Product -> Double
getPrice ( , , price) = price

getTotal :: Shop -> Double
getTotal shop = sum [qty * price | (_, qty, price) <- shop]

buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "not found"
buy name qty ((name1, qty1, _):ss)
    | name == name1 && qty < qty1  = (name1, 0, price1):ss
    | name == name1 && qty == qty1 = ss
    | name == name1 && qty > qty1  = p:ss
    | otherwise = buy name qty 

average :: [Double] -> Double
Average ps = sum ps / (fromIntegral (length ps))

getAverage :: Shop -> Double
getAverage shop = average (map getPrice shop)

closestToAverage :: Shop -> String
closestToAverage shop = fold1 (\p1@(_,_,pr1) p2@(_,_,pr2) -> if abs(averagePrice - pr1)) shop
    where averagePrice = average shop
