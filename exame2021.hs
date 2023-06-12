import Data.List 

{-
--1
myreplicate:: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x |n < 0 = []
                | otherwise = x : myreplicate (n-1) x

--2 
myintersect :: Eq a => [a] -> [a] -> [a]
myintersect l [] = []
myintersect [] l = []
myintersect l1 l2 | (head l1) /= (head l2) = myintersect (tail l1) (tail l2)
                          | otherwise = (head l1) : myintersect (tail l1) (tail l2)

--3
data LTree a = Tip a | Fork (LTree a) (LTree a)
data FTree a b = Leaf a | No b (FTree a b) (FTree a b)

conv :: LTree Int -> FTree Int Int
conv 


--uma soluçao, perceber??
conv :: LTree Int -> FTree Int Int
conv = snd . convAux

convAux :: LTree Int -> (Int, FTree Int Int)
convAux (Tip x) = (x, Leaf x)
convAux (Fork l r) = (s, No s ll rr)
    where (sl, ll) = convAux l
          (sr, rr) = convAux r
          s = sl + sr
-}
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x | n < 0 = []
                | otherwise = x : myreplicate (n-1) x

myintersect :: Eq a => [a] -> [a] -> [a] 
myintersect xs ys = filter (`elem` ys) xs  

data LTree a = Tip a | Fork (LTree a) (LTree a)
data FTree a b = Leaf a | No b (FTree a b) (FTree a b)

--conv :: LTree Int -> FTree Int Int
--conv 

type Mat a = [[a]]

--triSup :: Num a => Mat a -> Bool
--triSup mat = all (\(i, row) -> all (\(j, elem) -> i > j || elem == 0) $ zip [0..] row) $ zip [0..] mat

subseqSum :: [Int] -> Int -> Bool
subseqSum [] _ = False
subseqSum l k = any ((==k) . sum) (inits l) || subseqSum (tail l) k  