{-# LANGUAGE ScopedTypeVariables #-}
module Teste1 where


enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 a b 
	| a > b = []
	| a == b = [a]
	| otherwise = a : enumFromTo1 (a+1) b

enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c 
	| a > c = []
	| a == c = [a]
	| otherwise = a : enumFromThenTo1 (a-1+b) b  c

juntalistas :: [a] -> [a] -> [a]
juntalistas [] l = l
juntalistas (h:t) l =  h : juntalistas t l 

encontraIndiceLista  :: [a] -> Int -> a
encontraIndiceLista (h:t) a = if a == 0 then h else encontraIndiceLista t (a-1)

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

take1 :: Int -> [a] -> [a]
take1 a (h:t)
	| a >= length (h:t) = (h:t)
	| a-1 == 0 = [h]
	| otherwise = h : take1 (a-1) t

drop1 :: Int -> [a] -> [a]
drop1 a (h:t)
	| a >= length (h:t) = []
	| a == 0 = (h:t)
	| otherwise = drop1 (a-1) t

zip1 :: [a] -> [b] -> [(a,b)]
zip1 (h:t) (h1:t1) = (h,h1) : zip1 t t1
zip1 _ _ = []

elem1 ::  Eq a => a -> [a] ->Bool
elem1 a (h:t) = if a == h then True else elem1 a t 
elem1 _ _ = False

  
replicate1 ::  Int -> a ->[a] 
replicate1 a b = if a == 0 then [] else b : replicate1 (a-1) b

intersperse1 ::  a -> [a] ->[a]
intersperse1 a (h:t) = h : a : intersperse1 a t
intersperse1 _ _ = []

group1 ::  Eq a => [a] -> [[a]]
group1 (h:[]) = [[h]]
group1 (h:h1:t) = if h == h1 then (h:inicio):resto else [h]:inicio:resto where inicio:resto = group1 (h1:t)

concat1 :: Eq a => [[a]] -> [a]
concat1 (h:t)
	| (h:t) == [[]]  = []
	| t == [] = h
	| otherwise = h++concat1 t

inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = []: map (x:) (inits1 xs)-------------------------------------------------------------------------DUVIDA

tails1 ::  [a] -> [[a]]
tails1 (h:t) = reverse (inits1  (h:t))

isPrefixOf1 ::  Eq a => [a]-> [a] -> Bool  
isPrefixOf1 _ [] = False
isPrefixOf1 [] _ = True
isPrefixOf1 [] [] = True
isPrefixOf1 (h:t) (h1:t1) = if (h:t) == head (inits1 (h1:t1)) then True else isPrefixOf1 (h:t) t1 --a 







	






