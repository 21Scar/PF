import Data.Char
import Data.String
import Data.List
--1
enumFromT :: Int -> Int -> [Int]
enumFromT m n = if (m <= n) then (m :(enumFromT ( m + 1 ) n)) else []

enumFromT1:: Int -> Int -> [Int]
enumFromT1 x y = if (x<=y) then x:(enumFromT1 (x+1) y) else []


--2
enumFromThenTo1 :: Int -> Int -> Int -> [Int]
enumFromThenTo1 a b c
            |a < b && a >= c = []
            |a > b && a <= c = []
            |otherwise = a : enumFromThenTo1 b (2*b-a) c

--3
conca:: [a] -> [a] -> [a]
conca [] l = l
conca (h : t) l = h : conca t l

--4
encontra:: [a] -> Int -> a
encontra (h:_) 0 = h
encontra (_:t) n = encontra t (n-1)

--5
reverse1:: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse1 t ++ [h]

--6
take1:: Int -> [a] -> [a]
take1 _ [] =[]
take1 n (h:t) |n<=0 = []
              |otherwise = h : take1 (n-1) t

--7
drop1:: Int -> [a] -> [a]
drop1 _[] = []
drop1 n (h:t) |n<=0 = (h:t)
              |otherwise = drop1 (n-1) t

--8
zip1:: [a] -> [b] -> [(a,b)]
zip1 _[] = []
zip1 []_ = []
zip1 (h:t) (h':t') = (h,h') : zip1 t t'

--9
replicate1::Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x |n < 0 = []
               |otherwise = x : replicate1 (n-1) x

--10
myintersperse:: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [h] = [h]
myintersperse n (h:t) = h : n : myintersperse n t


--11
mygroup:: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup [x] = [[x]]
mygroup (h:t) | elem h (head r) = (h:(head r)) : tail r
              |otherwise = [h]:r 
              where r = mygroup t 

--12
concat1:: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = h ++ concat1 t

--13
inits1:: [a] -> [[a]]
inits1 []=[]
inits1 l= inits1 (init l) ++[l] 

--14
mytails :: [a] -> [[a]]
mytails [] = [[]]
mytails l = l : mytails (tail l )

--15
myheads :: [[a]] -> [a]
myheads [] = []
myheads ([]:t) = myheads t 
myheads (h:t) = head h : myheads t 

--16
total :: [[a]] -> Int
total [] = 0
total (h:t) = length h + total t

--17
fun :: [(a,b,c)] -> [(a,c)]
fun [] = []
fun ((a,b,c):t) = (a,c) : fun t 

--18
cola :: [(String,b,c)] -> String
cola [] = []
cola ((a,b,c) : t) = a ++ cola t

--rever

--19
idade :: Int -> Int -> [(String,Int)] -> [String]
idade x y ( (m,n) : t ) | x <= n = []
                        | x-n >= y = m : idade x y t
                        | otherwise = idade x y t 

--20
powerEnumFrom :: Int -> Int -> [Int] 
powerEnumFrom n 1 = [1]
powerEnumFrom n m| m > 1 = powerEnumFrom n (m-1) ++ [n^(m-1)]
                 |otherwise = []

--mypowerEnumFrom :: Int -> Int -> [Int]
--mypowerEnumFrom n m = aux n m 0
--aux n m expo = if expo < m then n expo : aux n m (expo+1) else []

--21
myisPrime :: Int -> Int -> Bool
myisPrime n m | n < 2 = False
              | myisPrimeAux n m = False
              | otherwise = True

myisPrimeAux :: Int -> Int -> Bool
myisPrimeAux n m = if (2 <= m && (m * m) <= n && mod n m == 0) then False else True

--22
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf l l1 | l == init l1 = True
                | otherwise = False

--23
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l l1 | l == tail l1 = True
                | otherwise = False

--24
myisSubsequenceOf :: Eq a => [a] -> [a] -> Bool
myisSubsequenceOf [] _ = True
myisSubsequenceOf _ [] = False
myisSubsequenceOf (h:t) (h':t') = if (h == h') then myisSubsequenceOf t t' else myisSubsequenceOf (h:t) t' 

--25
myelemIndices :: Eq a => a -> [a] -> [Int]
myelemIndices _ [] = []
myelemIndices n l = myelemIndicesAux n l 0

myelemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
myelemIndicesAux n (h:t) i | n == h = i : myelemIndicesAux n t (i+1)
                           | otherwise = myelemIndicesAux n t (i+1)
                where i = 0

--26
mynub :: Eq a => [a] -> [a]
mynub [] = []
mynub (h:t) = if elem h t then mynub t else h : mynub t  

--27
mydelete :: Eq a => a -> [a]-> [a]
mydelete _ [] = []
mydelete n (h:t) = if n == h then t else h : mydelete n t

--28
removerPrim:: Eq a => [a] -> [a] -> [a]
removerPrim [] _ = []
removerPrim l [] = l
removerPrim (h:t) (h':t') = if h == h' then removerPrim t t' else h : removerPrim t (h':t')

--29
myunion :: Eq a => [a] -> [a] -> [a]
myunion [] l = l
myunion l [] = l
myunion (h:t) (h':t') = if h == h' then myunion t t' else h : h' : myunion t t'
--union l (h:t)
--    | h `elem` l = union l t
--    | otherwise = union (l ++ [h]) t

--30
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect l [] = []
myintersect [] l = []
myintersect (h:t) (h':t') | h == h' = h : myintersect t t'
                        | otherwise = myintersect t t'

--31
myinsert :: Ord a => a -> [a] -> [a]
myinsert m [] = [m]
myinsert m (h:t) | m <= h = m:h:t
                 | otherwise = h : myinsert m t

--32
myunwords :: [String] -> String
myunwords [] = " "
myunwords (h:t) = if h /= last (h:t) then h ++ " " ++ myunwords t else h

--33
myunlines:: [String] -> String
myunlines [] = " "
myunlines (h:t) = if h /= last (h:t) then h ++ "\n" ++ myunlines t else h

--34
mypMaior :: Ord a => [a] -> Int
mypMaior [_] = 0
mypMaior (h:t) | h >= (t !! x) = 0
               | otherwise = 1 + x 
               where x = mypMaior t

--35
mylookup :: Eq a => a -> [(a,b)]-> Maybe b
mylookup _ [] = Nothing 
mylookup n ((a,b):t) | n == a = Just b 
                     | otherwise = mylookup n t  

--36
mypreCrescente :: Ord a => [a] -> [a]
mypreCrescente [] = []
mypreCrescente [x] = [x]
mypreCrescente (h:t) = if h < head t then h : mypreCrescente t else [h]

--37
myiSort :: Ord a => [a] -> [a]
myiSort [] = [] 
myiSort (h:t) = myinsert h (myiSort t)

--38
mymenor :: String -> String -> Bool
mymenor (h:t) (h':t') | h < h' = True
                      | h == h' = mymenor t t'
                      | otherwise =  False

--39
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet _ [] = False
elemMSet n ((a,b):t) | n == a = True
                     | otherwise = elemMSet n t 

--40
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,0):t) = converteMSet t 
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)  

--41
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet x [] = [(x,1)]
insereMSet x ((a,b):t) 
  | x == a = (a, b+1) : t 
  | otherwise = (a,b) : insereMSet x t

--42
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet x [] = []
removeMSet x ((a,b):t)
        | b == 1 && a == x = t
        | x == a = (a, b-1) : t
        | otherwise = (a,b) : removeMSet x t

--43
constroiMSet [] = []                             
constroiMSet (h:t) = constroiMSetAux t [h]       

constroiMSetAux [] acc = [(head acc, length acc)]
constroiMSetAux (h:t) acc | elem h acc = constroiMSetAux t (h:acc)
                          | otherwise = (head acc, length acc) : constroiMSetAux t [h]

--constroiMSet :: Ord a => [a] -> [(a,Int)]
--constroiMSet [] = []
--constroiMSet (h:t) = insereMSet h (constroiMSet t)

--constroiMSet (h:t) | h == head t = (h, a+1) : constroiMSet t
--                   | otherwise = (h, 1) : constroiMSet t

--constroiMSetAux :: Ord a => a -> (a,Int) -> (a,Int)
--constroiMSetAux a (b,c) = if (a == b) then (a,c + 1) else (a,c)

--44
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers ((Left a):t) = (a : as,bs)
    where (as,bs) = partitionEithers t
partitionEithers ((Right b):t) = (as,b : bs)
    where (as,bs) = partitionEithers t

--45
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing): t) = catMaybes t
catMaybes ((Just a ): t) = a : catMaybes t


data Movimento = Norte | Sul | Este | Oeste
                deriving Show

--46
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (a,b) (a',b') | (a' - a) > 0 = Este : caminho (a+1,b) (a',b')
                      | (a' - a) < 0 = Oeste : caminho (a-1,b) (a',b')
                      | (b' - b) > 0 = Norte : caminho (a,b+1) (a',b')
                      | (b' - b) < 0 = Sul : caminho (a,b-1) (a',b')
                      | otherwise = []

--47
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops _ [] = False
hasLoops (a,b) l = (a,b) == posicao (a,b) l || hasLoops (a,b) (init l)

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (a,b) [] = (a,b)
posicao (a,b) (Este:t) = posicao (a+1,b) t
posicao (a,b) (Oeste:t) = posicao (a-1,b) t
posicao (a,b) (Norte:t) = posicao (a,b+1) t
posicao (a,b) (Sul:t) = posicao (a,b-1) t

type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

--48
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados l = contaQuadradosAux l 0

contaQuadradosAux :: [Rectangulo] -> Int -> Int
contaQuadradosAux [] acc = acc
contaQuadradosAux ((Rect (x1,y1) (x2,y2)):t) acc = if (x2 - x1) == (y2 - y1) then contaQuadradosAux t (acc+1) else contaQuadradosAux t acc

--49
areaTotal :: [Rectangulo] -> Float
areaTotal l = areaTotalAux l 0

areaTotalAux :: [Rectangulo] -> Float -> Float
areaTotalAux [] acc = acc
areaTotalAux ((Rect (x1,y1) (x2,y2)):t) acc = areaTotalAux t ((x2-x1)*(y2-y1) + acc)

data Equipamento = Bom | Razoavel | Avariado
                deriving Show

--50
naoReparar :: [Equipamento] -> Int
naoReparar l = naoRepararAux l 0

naoRepararAux :: [Equipamento] -> Int -> Int 
naoRepararAux [] acc = acc
naoRepararAux (Bom:t) acc = naoRepararAux t (acc+1)
naoRepararAux (Razoavel:t) acc = naoRepararAux t (acc+1)
naoRepararAux (Avariado:t) acc = naoRepararAux t acc

--11 diferente
seguntatentativa:: Eq a => [a] -> [[a]]
seguntatentativa [] = []
seguntatentativa [x] = [[x]]
seguntatentativa (h:t) | elem h (head (seguntatentativa t)) = (h : head (seguntatentativa t)) : tail (seguntatentativa t)
                       | otherwise = [h]: seguntatentativa t 