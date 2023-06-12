
type MSet a = [(a,Int)]

--a)
converteMSet:: MSet a -> [a]
converteMSet [] = []
converteMSet ((a,1):t) = a : converteMSet t
converteMSet ((a,b):t) = a : converteMSet ((a,b-1):t)

--b)
removeMSet:: Eq a => a -> MSet a -> MSet a 
removeMSet n [] = []
removeMSet n ((a,b):t) | n == a = if b > 1 then ((a, b-1):t) else t
                       | otherwise = (a,b) : removeMSet n t
--c)
uniaoMSet :: Eq a => MSet a -> MSet a -> MSet a 
uniaoMSet l []= l
uniaoMSet [] l = l
uniaoMSet (x:xs) ys | fst x `elem` map fst ys = uniaoMSet xs (addElem (fst x) (snd x + snd (getElem (fst x) ys)) ys)
                    | otherwise = x : uniaoMSet xs ys
                where
                 getElem a (x:xs)
                    | fst x == a = x
                    | otherwise = getElem a xs       
                 addElem a n [] = [(a,n)]
                 addElem a n (x:xs)
                    | fst x == a = (a,n) : xs
                    | otherwise = x : addElem a n xs 


--2
type Posicao = (Int,Int)
data Movimento = Norte | Sul | Este | Oeste
data Caminho = C Posicao [Movimento]

{-
instance Eq Caminho where
  (C p1 ms1) == (C p2 ms2) = p1 == p2 && posFinal ms1 p1 == posFinal ms2 p2 && ms1 == ms2
    where posFinal [] (x,y) = (x,y)
          posFinal (m:ms) (x,y) = posFinal ms (move m (x,y))
          move Norte (x,y) = (x,y+1)
          move Sul (x,y) = (x,y-1)
          move Este (x,y) = (x+1,y)
          move Oeste (x,y) = (x-1,y)

-}

--3
func :: [[Int]] -> [Int]
func l = concat (filter (\x -> sum x >10) l)

myfunc :: [[Int]] -> [Int]
myfunc [] = []
myfunc (h:t) | sum h > 10 = h ++ myfunc t
             | otherwise = myfunc t


--4
data Prop = Var String | Not Prop | And Prop Prop | Or Prop Prop
p1 :: Prop
p1 = Not (Or (And (Not (Var "A")) (Var "B")) (Var "C"))

--a)
eval :: [(String,Bool)] -> Prop -> Bool
eval 

