-- Задача 1. Атака на Цезаровия шифър

-- Една от основните слабости на Цезаровия шифър се състои в това, че броят на възможните шифри е ограничен до броя на ротациите на буквите в азбуката минус едно. Това прави Цезаровия шифър податлив на т. нар. brute force атака, т. е. атака, която генерира всички възможни дешифровки на кодираното съобщение.
-- а). Напишете функцията crackall alphabet encrypted, която връща списък от всички възможни дешифровки на кодираното съобщение encrypted. 
-- Пример: crackall ['A'..'Z'] "FYYFHPQTSITSYTRTWWTBFYYJSFR" = ["EXXEGOPSRHSRXSQSVVSAEXXIREQ","DWWDFNORQGRQWRPRUURZDWWHQDP","CVVCEMNQPFQPVQOQTTQY CVVGPCO","BUUBDLMPOEPOUPNPSSPXBUUFOBN","ATTACKLONDONTOMORROWATTENAM","ZSSZBJKNMCNM SNLNQQNVZSSDMZL","YRRYAIJMLBMLRMKMPPMUYRRCLYK","XQQXZHILKALKQLJLOOLTXQQBKXJ","WPPW YGHKJZKJPKIKNNKSWPPAJWI","VOOVXFGJIYJIOJHJMMJRVOOZIVH","UNNUWEFIHXIHNIGILLIQUNNYHU G","TMMTVDEHGWHGMHFHKKHPTMMXGTF","SLLSUCDGFVGFLGEGJJGOSLLWFSE","RKKRTBCFEUFEKFDFII FNRKKVERD","QJJQSABEDTEDJECEHHEMQJJUDQC","PIIPRZADCSDCIDBDGGDLPIITCPB","OHHOQYZCBR CBHCACFFCKOHHSBOA","NGGNPXYBAQBAGBZBEEBJNGGRANZ","MFFMOWXAZPAZFAYADDAIMFFQZMY","LE ELNVWZYOZYEZXZCCZHLEEPYLX","KDDKMUVYXNYXDYWYBBYGKDDOXKW","JCCJLTUXWMXWCXVXAAXFJCCN WJV","IBBIKSTWVLWVBWUWZZWEIBBMVIU","HAAHJRSVUKVUAVTVYYVDHAALUHT","GZZGIQRUTJUTZUSU XXUCGZZKTGS"]
-- б). След като сме генерирали всички възможни дешифровки, бихме могли лесно да намерим най-вероятните от тях, използвайки факта, че някои кратки думи, напр. the, at, on, се срещат много често в английския език. За тази цел най-напред напишете функция substring sub str, която проверява дали поднизът sub се среща в низа str. 
-- Пример: substring "Haskell" "Haskell Curry" = True substring "Curry" "Haskell Curry" = True substring "Turing" "Haskell Curry" = False
-- в). Използвайте функциите от предишните две подточки, за да напишете функцията crackcandidates alphabet commonwords encrypted, която приема списък с често срещани думи и криптирано съобщение и връща списък с потенициални вероятни разшифровки. Една разшифровка се смята за вероятна, ако съдържа поне една от думите от списъка с често срещани думи. 
-- Пример: crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR" = ["ATTACKLONDONTOMORROWATTENAM"]

-- Задача 2. Polysubstitution cypher (шифър с множествено заместване)
-- Един от простите начини да се справим със слабостта на Цезаровия шифър е да разбием съобщението на блокове от по няколко знака и да криптираме всеки от тях с различен Цезаров шифър, отместен с определена стъпка спрямо предишния.
-- а). Напишете функция polyencrypt alphabet offset step blockSize normalized, която приема азбука alphabet, първоначално отместване offset, стъпка step и размер на блока blockSize, както и съобщение в нормализиран вид, и връща криптирано съобщение, първите blockSize знака на което са криптират с отместване offset, следващите blockSize знака - с отместване offset + step, и т. н.
-- Пример: polyencrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM" = "FYYFHPQUTJUTZUTVYYVDHBBMVIU"
-- б). Напишете функция polydecrypt alphabet offset step blockSize encrypted, която декриптира съобщението от предишната подточка.
-- Пример: polydecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU" = "ATTACKLONDONTOMORROWATTENAM"

-- Задача 3. Емулация на Eнигма Един от основните компоненти на Енигма е система от ротори, всеки от които може да се моделира като polysubstitution cypher от предната задача. Резултатът от всеки от роторите се предава като вход на следващия. Резултатът от последния ротор е криптираното съобщение. 
-- а). Напишете функция enigmaencrypt alphabet rotors normalized, която приема азбука alphabet, списък от ротори (offset, step, blockSize) и съобщение в нормализиран вид и връща криптираното от роторите съобщение. 
-- Пример: enigmaencrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ATTACKLONDONTOMORROWATTENAM" = "ZTUCFOQUULZZGCBEIJHQXRSEOFS" 
-- б). Напишете функция enigmadecrypt alphabet rotors normalized, която приема азбука, списък от ротори и криптирано съобщение и връща оригиналното съобщение.
-- Пример: enigmadecrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ZTUCFOQUULZZGCBEIJHQXRSEOFS" = "ATTACKLONDONTOMORROWATTENAM"
import Data.List

main :: IO()
main = do
    --task1
    print(substring "Haskell" "Haskell Curry") -- True
    print(substring "Curry" "Haskell Curry") -- True
    print(substring "Turing" "Haskell Curry") -- False
    print(crackcandidates ['A'..'Z'] ["THE","AND","AT","ON","IS"] "FYYFHPQTSITSYTRTWWTBFYYJSFR")
    --task2
    print(polyencrypt ['A'..'Z'] 5 1 7 "ATTACKLONDONTOMORROWATTENAM")
    print(polydecrypt ['A'..'Z'] 5 1 7 "FYYFHPQUTJUTZUTVYYVDHBBMVIU")
    --task3
    print(enigmaencrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ATTACKLONDONTOMORROWATTENAM")
    print(enigmadecrypt ['A'..'Z'] [(5,1,1),(7,2,10),(13,3,25)] "ZTUCFOQUULZZGCBEIJHQXRSEOFS")

--task1

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

--today
crackall :: [Char] -> String -> [[Char]]
crackall_alphabet_encrypted alphabet = [decrypt alphabet o encrypted | o<-[1 .. 25]]

substring :: String -> String -> Bool
substring "" "" = True
substring _ "" = False
substring (_:a1:a2:_) (_:b1:b2:_) = a1 == b1 && a2 == b2

crackcandidates :: [Char] -> [String] -> String -> [Char]
crackcandidates alphabet commonwords encrypted = [decrypted | decrypted <- crackall alphabet encrypted any (\ -> substring) ]

--task2
polyencrypt :: [Char] -> Int -> Int -> Int -> String -> String
polyencrypt _ _ _ "" = ""
polyencrypt alphabet offset step blockSize normalized = encrypt alphabet take (bloackSize normalized) ++ polyendcrypt alphabet take (offset + step)
step blockSize (drop blockSize normalized)

polydecrypt :: [Char] -> Int -> Int -> Int -> String -> String
polydecrypt _ _ _ "" = ""
polydecrypt alphabet offset step blockSize normalized = dencrypt alphabet take (bloackSize normalized) ++ polydecrypt alphabet take (offset + step)
step blockSize (drop blockSize normalized)

--task3
enigmaencrypt :: [Char] -> [Int] -> String -> String
enigmaencrypt alphabet rotors@(x:xs) normalized = 
    foldr (\ (offset, step, blockSize) temp -> polyencrypt aplhabet offset step blockSize  normalized ) temp normalized rotor 