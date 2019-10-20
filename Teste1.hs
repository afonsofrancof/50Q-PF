module Teste1 where

data Semaforo = Verde | Amarelo | Vermelho deriving (Show)


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
concat1 (h:t) | (h:t) == [[]] = [] 
 			  | t == [] = h 
			  | otherwise = h++concat1 t 

inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = []: map (x:) (inits1 xs)

tails1 ::  [a] -> [[a]]
tails1 (h:t) = reverse (inits1  (h:t))

isPrefixOf1 :: Eq a => [a] -> [a] -> Bool
isPrefixOf1 [] _ = True
isPrefixOf1 _ [] = False
isPrefixOf1 (x:xs) (y:ys) = if x == y && isPrefixOf1 xs ys then True else False

isSufixOf1 :: Eq a => [a] -> [a] -> Bool
isSufixOf1 [] _ = True
isSufixOf1 _ [] = False
isSufixOf1 (x:xs) (y:ys) = isPrefixOf1 (reverse (x:xs)) (reverse (y:ys))


isSubsequenceOf1 :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf1 [] _ = True
isSubsequenceOf1 _ [] = False
isSubsequenceOf1 (h:t) (h1:t1) = isSubsequenceOf1 (if h == h1 then t else h:t) t1

elemIndices1 ::  Eq a => a ->[a] -> [Int]
elemIndices1 _ [] = []
elemIndices1 a (h:t) = if a == h then 0 : map (+1) (elemIndices1 a t) else map (+1) (elemIndices1 a t)

nub1 ::  Eq a => [a] -> [a]
nub1 (h:t) = h : filter (/=h) (nub1 t)
nub1 [] = []

delete1 ::  Eq a => a -> [a]-> [a]
delete1 _ [] = [] 
delete1 a (h:t) = if a == h then t else h : (delete1 a t)

delete1listas ::  Eq a => [a] -> [a] -> [a]
delete1listas (h:t) (h1:t1) = delete1listas (delete1 h1 (h:t)) t1
delete1listas [] _ = []
delete1listas (h:t) [] = (h:t)

union1 ::  Eq a => [a] -> [a]-> [a]
union1 [] (h1:t1) = (h1:t1)
union1 l (h1:t1) = if (h1 `elem` l) then union1 l t1 else union1 (l++[h1]) t1
union1 l [] = l

intersect1 ::  Eq a => [a] ->[a] -> [a]
intersect1 (h:t) l = if h `elem` l then h : (intersect1 t l) else intersect1 t l
intersect1 [] l = []

insert1 ::  Ord a => a -> [a]-> [a]
insert1 a l = undefined 











	






