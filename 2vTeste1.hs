module Teste1v2 where


enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b = if a == b then [b] else a:enumFromTo1 (a+1) b -- 1 - Esquecime dos parênteses á volta do a+1

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c  | a > c = []
					   | a == c = [c] 
					   | otherwise = a:enumFromThenTo1 (a-1+b) b c -- 1 - enganei-me e pus a == b em vez de a == c 
 																   -- 2 - coloquei a:(a+b):enum.... em vez de apenas a:enum...
 																   -- 3 - esquecime da opção a > c , porque não vai de 1 em 1


-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

juntalistas1 :: [a] -> [a] -> [a]
juntalistas1 [] a = a
juntalistas1 a [] = a 
juntalistas1 (h:t) (h1:t1) = h:juntalistas1 t (h1:t1) -- 1 - esquecime que ao ficar [] (h1:t1) , ele dá logo (h1:t1) , e eu coloquei depois.

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = juntalistas1 (reverse1 t) [h]

juntalistas2 :: [a] -> [a] -> [a]
juntalistas2 [] a = a
juntalistas2 a [] = a 
juntalistas2 (h:t) (h1:t1) = h:juntalistas2 t (h1:t1)

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

take1 :: Int -> [a] -> [a]
take1 a (h:t) = if a == 0 then [] else h : take1 (a-1) (t)

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

drop1 :: Int -> [a] -> [a]
drop1 a (h:t) = if a == 0 then (h:t) else drop1 (a-1) t    -- 1 - esquecime de escrever "then" entre o "0" e o "(h:t)"
                                                           -- 2 - escrevi take1 em vez de drop1
                                                           -- 3 - escrevi [] em vez de (h:t)

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

zip1 :: [a] -> [b] -> [(a,b)]
zip1 (h:t) (h1:t1) = (h,h1):zip1 t t1
zip1 _ _ = [] 

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

elem1 :: Eq a => a -> [a] -> Bool
elem1  a (h:t) = if a == h then True else elem1 a t  -- 1 - Esquecime do elem1 a [] = False
elem1 a [] = False

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

replicate1 :: Int -> a -> [a]
replicate1 n x = if n == 0 then [] else x:replicate1 (n-1) x 

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

intersperse1 :: a -> [a] -> [a]
intersperse1 a (h:t) = h:a:intersperse1 a t -- 1 - esquecime do intersperse1 _ _ = []
intersperse1 _ _  = []

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

group1 :: Eq a => [a] -> [[a]]
group1 (h:[]) = [[h]]
group1 (h:h1:t) = if h == h1 then (h:inicio):resto else [h]:inicio:resto where inicio:resto = group1 (h1:t) -- 1- esquecime completamente como fazer este

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

concat1 :: [[a]] -> [a]
concat1 (h:t) = h++concat1 t
concat1 _ = [] 

-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 l =  juntalistas3 (inits1 (tudomenosultimo2 l)) [l] 

tudomenosultimo2 :: [a] -> [a]
tudomenosultimo2 (h:t) = reverse2 (tail (reverse2 (h:t)))

reverse2 :: [a] -> [a]
reverse2 [] = []
reverse2 (h:t) = juntalistas3 (reverse2 t) [h]

juntalistas3 :: [a] -> [a] -> [a]
juntalistas3 [] a = a
juntalistas3 a [] = a 
juntalistas3 (h:t) (h1:t1) = h:juntalistas3 t (h1:t1)

---------------------------------------------------------------------------------------------------------------------------------------------------------------

tails1 :: [a] -> [[a]]
tails1 [] = [[]]
tails1 (h:t) =(h:t):(tails1 t)

---------------------------------------------------------------------------------------------------------------------------------------------------------------

isPrefixOf1 :: Eq a => [a] -> [a] -> Bool 
isPrefixOf1 [] _ = True
isPrefixOf1 _ [] = False
isPrefixOf1 (h:t) (h1:t1) = if h == h1 && isPrefixOf1 t t1 then True else False 

---------------------------------------------------------------------------------------------------------------------------------------------------------------

isSuffixOf1 :: Eq a => [a] -> [a] -> Bool 
isSuffixOf1 (h:t) (h1:t1) = isPrefixOf1 (reverse3 (h:t)) (reverse3 (h1:t1))

reverse3 :: [a] -> [a]
reverse3 [] = []
reverse3 (h:t) = juntalistas4 (reverse3 t) [h]

juntalistas4 :: [a] -> [a] -> [a]
juntalistas4 [] a = a
juntalistas4 a [] = a 
juntalistas4 (h:t) (h1:t1) = h:juntalistas4 t (h1:t1)

---------------------------------------------------------------------------------------------------------------------------------------------------------------






