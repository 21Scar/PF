import Data.Char

--ex1 a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)
--funA [2,3,5,1] = 39.0 -> soma do quadrado dos elementos da lista; 2²+3²+5²+1²=39.0

--b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t) else (funB t)
{-funB [8,5,12] = [8,12] -> se o resto da divisao inteira de h (primeiro elemento da lista) por 2 for igual a 0 entao esse
elemento fará parte da lista resultado, e a funçao irá verificar o resto dos numeros da lista de entrada, para ver se tambem
verificam a condiçao. Caso nao se registe que a condiçao é verificada entao descarta-se esse elemento e verificasse o resto
da lista. No exemplo, 8/2=4 com resto = 0 entao faz parte da lista, assim como 12/2=6 resto=0. No entanto, o resto de 5/2 é
diferente de 0 logo nao faz parte e a lista resultante da funçao é  [8,12]. (basicamente a lista resultado sao numeros pares)-}

--c)
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []
{-funC [1,2,3,4,5] = [5]
No exemplo a funçao deteta o primeiro caso, dois elementos (1,2) e depois a cauda da lista (3,4,5). Como deteta esse primeiro caso,
irá fazer o que esta programado para esse, "remove" ou "esquece" os dois primeiros elementos e volta a chamar a funçao para a cauda 
da lista. Restam agora (3,4,5), ou seja, dois elementos (3,4) e a cauda da lista (5). Como é o mesmo caso inicial, repete o processo,
mas neste caso sobra apenas um elemeneto [5]. Passamos para o caso a seguir ([x]=[x])ou seja a lista final será [5].
(se a lista tiver um nº impar de elementos o resultado sera uma lista com o ultimo elemento da lista dada [x], no caso de ser nº par
o resultado sera a lista vazia []) 
-}

--d)
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t
{-funD "otrec" = "certo"
se lista vazia = lista vazia
se String (lista de caracteres) = lista no sentido inverso (ultimo elemento para o primeiro lugar, etc)-}

--2 a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

--b)
numOcorre :: Char -> String -> Int
numOcorre _ "" = 0
numOcorre c (h:t) | c==h = 1 + numOcorre c t
                  | otherwise = numOcorre c t 

--c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = if h<=0 then False else positivos t

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h>0 = soPos t
            | otherwise = h : soPos t                   

--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | h<=0 = h + somaNeg t
              | otherwise = somaNeg t

--f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length t < 3 = h:t
              | otherwise = tresUlt t

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = y : segundos t

--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros n ((x,y):t) | n==x = True
                         | otherwise = nosPrimeiros n t

--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x+x',y+y',z+z')
                    where (x',y',z') = sumTriplos t

--3 a) ??? 
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) | isDigit h = h : soDigitos t
                | otherwise = soDigitos t


--3 b)
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t) = if isLower h == True then 1 + minusculas t else minusculas t
                   where n = 0 

--3 c)
nums :: String -> [Int]
nums [] = []
nums (h:t) = if isDigit h == True then digitToInt h : nums t else nums t
--isDigit vê se é um digito, digitToInt transforma Char em Int, isLower vê se a letra é maiscula ou minuscula (True se for minuscula)


type Polinomio = [Monomio]
type Monomio = (Float,Int)

--4 a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y) : t) = if n == y then 1 + conta n t else conta n t  

--4 b)
grau :: Polinomio -> Int
grau [] = 0
grau ((x,y) : t) | y > grau t = y
                 | otherwise = grau t

--4 c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((x,y):t) | n == y = ((x,y) : selgrau n t)
                    | otherwise = selgrau n t  

-- 4 d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) |y == 0 = deriv t
                |otherwise = ((x* fromIntegral y),(y-1)) : deriv t --will convert from any Integral type into any Num eric type (which includes Int , Integer , Rational , and Double ): fromIntegral :: (Num b, Integral a) => a -> b.

--4 e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n ((x,y) : t) = x * n^y + calcula n  t

--4 f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if y == 0 then (x,y) : simp t else simp t

-- 4 g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((x1,y1):t) = (x * x1, y + y1) : mult (x,y) t

-- 4 h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((x,y):t) = normalizaAux (x,y) (normaliza t)

normalizaAux :: Monomio -> Polinomio -> Polinomio
normalizaAux m [] = [m]
normalizaAux (x1,y1) ((x,y):t) | y1 == y = (x1 + x,y) : t
                               | otherwise = (x,y) : normalizaAux (x1,y1) t 
{-
--4 i)
soma :: Polinomio -> Polinomio -> Polinomio
soma 
-}

