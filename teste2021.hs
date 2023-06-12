type Mat a = [[a]]

--1
myzip :: [a] -> [b] -> [(a,b)]
myzip [] [] = []
myzip [] _ = []
myzip _ [] = []
myzip (x:t) (xs: ts) = ((x,xs): myzip t ts)

--2
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (x:t) | x < head t = x : preCrescente t
                     | otherwise = [x]

--3
amplitude :: [Int]-> Int
amplitude [] = 0
amplitude l = maximum l - minimum l

--4
somaMat:: Num a => Mat a -> Mat a -> Mat a
somaMat zipWith . zipWith $ (+)

--5 
type Nome = String
type Telefone = Integer
data Agenda = Vazia | Nodo (Nome,[Telefone]) Agenda Agenda

