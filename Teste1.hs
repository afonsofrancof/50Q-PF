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
insert1 a (h:t) | a < h = a:h:t 
				| otherwise = h:insert1 a t
insert1 a [] = [a]


unwords1 ::  [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h++(if t == [] then "" else " ")++(unwords1 t)


unlines1 ::  [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h++(if t == [] then "" else "\n")++(unlines1 t)


pMaior1 ::  Ord a => [a] -> Int
pMaior1 (h:[]) = 0
pMaior1 (h:t) = if h > (t!!pMaior1 t) then 0 else 1+pMaior1 (t)


temRepetidos1 ::  Eq a => [a] -> Bool
temRepetidos1 [] = False
temRepetidos1 (h:t) = h `elem` t || temRepetidos1 t 


algarismos1 ::  [Char] -> [Char]
algarismos1 (h:t) | h `elem` ['1'..'9'] = h:algarismos1 t
				  | otherwise = algarismos1 t 
algarismos1 [] = []


posImpares1 ::  [a] -> [a]
posImpares1 (h:h1:t) = h1:posImpares1 t
posImpares1 [] = []


posPares1 ::  [a] -> [a]
posPares1 [] = []
posPares1 (h:h1:t) = h:posPares1 t


isSorted1 ::  Ord a => [a] -> Bool
isSorted1 (h:h1:t) = h <= h1 && isSorted1 (h1:t) 
isSorted1 (h:[]) = True
isSorted1 [] = True


iSort1 ::  Ord a => [a] -> [a]
iSort1 [] = []
iSort1 (h:t) = insert1 h (iSort1 t) 


menor1 ::  String -> String -> Bool
menor1 (h:t) (h1:t1) = h < h1 || menor1 t t1 
menor1 _ "" = False
menor1 "" _ = True


elemMSet1 ::  Eq a => a -> [(a,Int)] -> Bool
elemMSet1 a ((b,c):t) = b == a || elemMSet1 a t
elemMSet1 a [] = False


lengthMSet1 ::  [(a,Int)] -> Int
lengthMSet1 ((a,b):t) = b+lengthMSet1 t
lengthMSet1 [] = 0


converteMSet1 ::  [(a,Int)] -> [a]
converteMSet1 [] = []
converteMSet1 ((a,b):t) = replicate1 b a++converteMSet1 t


insertMSet1 ::  Eq a => a -> [(a,Int)] -> [(a,Int)]					   
insertMSet1 a [] = [(a,1)]
insertMSet1 a ((b,c):t) = if a == b then ((b,c+1):t) else (b,c):insertMSet1 a t


removeMSet1 ::  Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet1 a [] = []
removeMSet1 a ((b,c):t) = if a == b then (if c == 1 then t else (b,c-1):t) else (b,c):removeMSet1 a t


constroiMSet1 ::  Ord a => [a] -> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 (h:t) = insertMSet1 h (constroiMSet1 t)


partitionEithers1 ::  [Either a b] -> ([a],[b])
partitionEithers1 l = (partleft l, partright l)
	where
		partleft [] = []
		partleft ((Left x):t) = x:partleft t
		partleft ((Right x):t) = partleft t
		partright [] = [] 
		partright ((Left x):t)= partright t
		partright ((Right x):t) = x:partright t


catMaybes ::  [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just a):t) = a:catMaybes t
catMaybes (Nothing:t) = catMaybes t

data Movimento = Norte | Sul | Este | Oeste deriving (Show,Eq) -------------------------------------- Só me dá se adicionar o Eq ao dereving (Show)

posicao1 :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao1 p [] = p
posicao1 (x,y) (h:t) | h == Norte = posicao1 (x, y + 1) t
                     | h == Sul =   posicao1 (x, y - 1) t
                     | h == Este =  posicao1 (x + 1, y) t
                     | h == Oeste = posicao1 (x - 1, y) t


caminho1 ::  (Int,Int) -> (Int,Int) -> [Movimento]
caminho1 (x,y) (x1,y1) | x1 > x = Este:caminho1 (x+1,y) (x1,y1)
					   | x > x1 = Oeste:caminho1 (x-1,y) (x1,y1)
					   | y1 > y = Norte:caminho1 (x,y+1) (x1,y1)
					   | y > y1 = Sul:caminho1 (x,y-1) (x1,y1)
					   | otherwise = []


vertical1 ::  [Movimento] -> Bool
vertical1 [] = True
vertical1 (h:t) = if h == Este || h == Oeste then False else vertical1 t

data Posicao = Pos Int Int deriving (Show)

maisCentral1 ::  [Posicao] -> Posicao
maisCentral1 [(Pos x y)] = (Pos x y)
maisCentral1  ((Pos x y):(Pos x1 y1):t) = if (x^2)+(y^2) > (x1^2)+(y1^2) then (Pos x y) else maisCentral1 ((Pos x1 y1):t)

vizinhos1 :: Posicao -> [Posicao] -> [Posicao]
vizinhos1 _ [] = []
vizinhos1 (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                       then (Pos xv yv):vizinhos1 (Pos x y) ps 
                                       else vizinhos1 (Pos x y) ps


-- falta 48


mesmaOrdenada1 ::  [Posicao] -> Bool
mesmaOrdenada1 [] = True
mesmaOrdenada1 ((Pos x y):(Pos x1 y1):t) = if y == y1 then mesmaOrdenada1 ((Pos x1 y1):t) else False
mesmaOrdenada1 ((Pos x y ):t) = True 

data Semaforo = Verde | Amarelo | Vermelho deriving (Show)

interseccaoOK ::  [Semaforo] -> Bool
interseccaoOK a = undefined         ---------------------------------- Falta este também (50) 

	






 






	






