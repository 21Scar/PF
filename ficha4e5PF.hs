--Funcoes de ordem superior 10/11/2022
map:: (a-> b) -> [a] -> [b]

-- map (2*) [1,3,5] = [2,6,10]
filter :: (a -> Bool) -> [a] -> [b]
--filter (>5) [1,10,2,30) = [10,30]

foldl:: (a->b->a) -> a -> [b] -> a
foldl f a []= a
foldl f a (h:t) = 

--foldl (/) 64 [4,2,4]
--fold (/) (64/2) [4]
--foldl (/) (8/9) []=2

--foldl max 0 [5,9,2]
--foldl max (max 0 5) [9,2]
--foldl max 5 [9,2]
--foldl max 9 [2]
--foldl max 9 []= 9

--Ficha 4
--exercicio1
digitAlpha :: String -> (String,String)
digitAlpha s = (filter isAlpha s, filter isDigit s)

--ou

digitAlpha1 :: String -> (String ->(String, String)
digitAlpha1 " "= 
digitAlpha1 (h:t) | isAlpha1 h = (h:)
				  | isDigit isAlphadigitAlpha isAlpha
				  | isDigit isAlphadigitAlpha
				  | otherwise = (as:ds)
				  where (as:ds)= digitAlpha1 t 

isDigit :: Striq > (String, String) 
isDigit foldl isDigitAux ("","") s
where isDigitAux :: (String,String) -> Char -> (String,String)
isDigitAux

isDigit (as,ds)| isAlpha c =(as++[e], ds) 
			   | isDigit c = c (as,ds++[e]) 

			   |otherwise = (as,adidas)
--digitAlpha "o2eb!3" = ("ocb", "2,3") h digitAlpha1
-- isDigitSAux:: (String,String) -> 
-- ("",","") ··= ("o", "")  


--exercício2
nzp:: [Int] -> (Int,Int,Int)
nzp l = foldl aux (0,0,0) l 
	where aux :: (Int,Int,Int) -> n -> (Int,Int,Int)
			aux (n,z,p) x | x < 0 = (n+1,z,p)
						  | x== 0 = (n,z+1,p)
						  | x > 0 = (n,z,p+1)

-- npz [1,-1,1]
-- foldl aux (o,o,o) [1,-1,1]
-- foldl aux (0,0,1) [-1,1]
-- foldl aux (1,0,1) [1]
-- foldl aux (1,0,1) [] = (1,0,2)

--funcoes anonimas 
--nzpl= fold (\(nzp-> if (x = 0))


--Listas de compreensao 
-- [x | x <- [1...20], mod x2==0, mod x 3 ==0]


--


