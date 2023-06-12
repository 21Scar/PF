--28 
barraBarra :: Eq a => [a] -> [a]-> [a]
barraBarra [] l = []
barraBarra l [] = l
barraBarra (h:t) (h1:t1) | h == h1 = barraBarra t t1
                         | otherwise = h : barraBarra t (h1:t1)

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade _ _ [] = []
idade dataa1 anos ((nome,dataa2):t) | ((abs (dataa1 - dataa2)) >= anos) = nome : idade dataa1 anos t
                                    | otherwise = idade dataa1 anos t
--5
myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = myreverse t ++ [h]

--45 
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just h:t) = h : catMaybes t
catMaybes (Nothing : t) = catMaybes t

--3
concatena :: [a] -> [a] -> [a]
concatena [] l = l
concatena (h:t) l = h : concatena t l

--47
{-
data Movimento = Norte | Sul | Este | Oeste
deriving Show
hasLoops :: (Int,Int) -> [Movimento] -> Bool
-}
--26
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (h:t) | elem h t == True = mynub t 
            | otherwise = h : mynub t

--44
--data Either a b = Left a | Right b
mypartitionEithers :: [Either a b] -> ([a],[b])
mypartitionEithers [] = ([],[])
mypartitionEithers (x:t)= case x of
 Left a -> (a:as,bs)
 Right b -> (as,b:bs)
 where (as,bs) = mypartitionEithers t  

--25
myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices n l = myelemIndicesAux n l 0

myelemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
myelemIndicesAux x (h:t) i = if x == h then (i: myelemIndicesAux x t (i+1)) else myelemIndicesAux x t (i+1)
 where i = 0 

--9
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = x: myreplicate (n-1) x 

--12
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (h:xs) = h ++ myconcat xs

--21 nao da direito
isPrime :: Int -> Bool
isPrime n = n >= 2 && isPrimeAux n 2 

isPrimeAux :: Int -> Int -> Bool
isPrimeAux n m | m*m > n = True
               | mod n m == 0 = False
               | otherwise = False

--31
myinsert :: Ord a => a -> [a]-> [a]
myinsert n [] = [n] 
myinsert n (h:t) | n <= h = n : h : t 
                 | otherwise = h : myinsert n t
 

