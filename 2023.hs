



--1
eenumFromTo x y = [x..y]

--2
eenumFromThenTo x space y = [x,x+(space-1)..y]

--3

addLists a [] = a
addLists [] a = a
addLists (h:t) (h1:t1) = h : addLists t (h1:t1)

--4
bangbang (h:t) index = if index == 0 then h else bangbang t (index-1)

--5
rreverse [] = []
rreverse (h:t) = rreverse t ++ [h] 

--6
ttake num [] = []
ttake num (h:t) | num > 0   =  h : ttake (num-1) t
                | otherwise = []

--7
ddrop num [] = []
ddrop num (h:t) | num > 0   = ddrop (num-1) t
                | otherwise = h:t

--8
zzip a [] = [] 
zzip [] a = []
zzip (h:t) (h1:t1) = (h,h1):zzip t t1

--9
eelem e [] = False
eelem e (h:t) | e == h    = True
              | otherwise = eelem e t

--10
rreplicate 0 val = []
rreplicate times val = val : rreplicate (times-1) val 


--11
iintersperce e [] = []
iintersperce e (h:t) = h:e: iintersperce e t


--12
ggroup [h] = [[h]]
ggroup (h:h1:t) = if h == h1 then (h:inicio):resto else [h]:inicio:resto
    where
        inicio:resto = ggroup (h1:t)

--13
cconcat l = foldr (++) [] l

--14
iinits [] = [[]]
iinits l = iinits (init l) ++ [l]

--15
ttails [] = [[]]
ttails l = l : ttails (tail l) 

--16
isPrefixOf [] l = True
isPrefixOf (h:t) (h1:t1) | h /= h1 = False
                         | otherwise = isPrefixOf t t1

--17
isSuffixOf [] l = True
isSuffixOf l1 l2 | last l1 /= last l2 = False
                 | otherwise = isSuffixOf (init l1) (init l2)

--18
iisSubsequenceOf [] _ = True   
iisSubsequenceOf _ [] = False
iisSubsequenceOf (h:t) (h1:t1) | h == h1 = iisSubsequenceOf t t1
                               | otherwise = iisSubsequenceOf (h:t) t1

--19
eelemIndices e (h:t) = if e == h then 0 : map (+1) (eelemIndices e t) else map (+1) (eelemIndices e t)

--20
nnub [] = []
nnub (h:t) = if h `elem` t then nnub t else h: nnub t

--21
ddelete e (h:t) = if h == e then t else h : ddelete e t

