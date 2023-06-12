import Data.List
--1 a)
myany :: (a -> Bool) -> [a] -> Bool
myany f [] = False
myany f (h:t) | f h = True
              | otherwise = myany f t


--b)
myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f (h:t) (h1:t1) = f h h1 : myzipWith f t t1
myzipWith _ _ _ = []


--c)
mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile f [] = []
mytakeWhile f (h:t) | f h == True = h : mytakeWhile f t
                    | otherwise = []

--d)
mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile f [] = []
mydropWhile f (h:t) | f h == True = mydropWhile f t
                    | otherwise = h : t

--e)
myspan :: (a-> Bool) -> [a] -> ([a],[a])
myspan _ [] = ([],[])
myspan f (h:t) | f h = let (taken, dropped) = myspan f t in (h:taken, dropped)
               | otherwise = ([],h:t)


--f)
mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy _ _ [] = []
mydeleteBy f x (h:t) | f x h == True = t
                     | otherwise = h : mydeleteBy f x t 

--g) ajuda a entender??
mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn f [] = []
mysortOn f (h:t) = myinsertOn f h (mysortOn f t)

myinsertOn :: (Ord b) => (a -> b) -> a -> [a] -> [a]
myinsertOn _ x [] = [x]
myinsertOn f x (h:t) = if f x > f h then h : myinsertOn f x t else x : h : t

--2 a)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau g l = filter (\ x -> snd x == g) l

--b)
conta :: Int -> Polinomio -> Int
conta n p = length (selgrau n p)

--c)
{-ara resolver esse problema, você pode usar a função maximum de ordem superior em conjunto com a função map para encontrar o expoente 
máximo de todos os monômios em um polinômio.A função map aplica a função snd (que retorna o segundo elemento de um par) a cada monômio 
no polinômio p. O resultado é uma lista de expoentes. Em seguida, a função maximum encontra o maior expoente nessa lista, que é o grau 
do polinômio. -}
grau :: Polinomio -> Int
grau p = maximum (map snd p) 

--d)
deriv :: Polinomio -> Polinomio
deriv p = [(c * fromIntegral g,(g-1)) | (c,g) <- p, g /=0]

--e)
{-Para calcular o valor de um polinômio para um determinado valor de x, você pode usar a função map e a função sum de ordem superior em conjunto. 
A  função map aplica a expressão (\(a,b) -> a * x^b) a cada monômio no polinômio p, produzindo uma lista de valores. A função sum então soma esses
valores para produzir o valor final do polinômio para o valor de x dado.-}

calcula :: Float -> Polinomio -> Float
calcula x p = sum (map (\(a,b) -> a * x^b) p)

--f)
simp :: Polinomio -> Polinomio
simp p = filter (\ x -> snd x /= 0) p

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult (am,bm) p = map (\(a,b) -> (a*am, b+bm)) p

--h)
ordena :: Polinomio -> Polinomio
ordena p = sortBy compare p

--i)
{-A função normaliza pode ser implementada usando funções de ordem superior combinadas 
com o agrupamento de monômios com o mesmo grau e a soma de seus coeficientes
Neste exemplo, a função normaliza primeiro classifica o polinômio p em ordem decrescente de grau 
usando a função sortBy e a função maiorGrau. Em seguida, o polinômio classificado é agrupado usando a função 
groupBy e a função igualGrau para identificar monômios com o mesmo grau. Finalmente, a função map é usada para aplicar a função 
somaMonomios a cada grupo, onde a função somaMonomios soma os coeficientes de cada monômio e retorna um único monômio com o grau resultante.-}

normaliza :: Polinomio -> Polinomio
normaliza p = map somaMonomios agrupados
  where agrupados = groupBy igualGrau (sortBy maiorGrau p)
        igualGrau (_,e1) (_,e2) = e1 == e2
        maiorGrau (_,e1) (_,e2) = compare e2 e1
        somaMonomios ps = (sum (map fst ps), snd (head ps))

--j)
{-A função utiliza a função ++ para concatenar as duas listas de monômios dos polinômios de entrada 
e depois utiliza a função map para remover monômios com graus iguais e somar os coeficientes.
Por fim, utilizamos a função normaliza para normalizar o polinomio resultante
A função soma soma os polinômios p1 e p2. O resultado é um novo polinômio com os monômios ordenados e sem monômios com graus iguais.-}

--soma :: Polinomio -> Polinomio -> Polinomio
--soma p1 p2 = normaliza $ map (\(c,e) -> (c,e)) (p1 ++ p2)

--3

type Mat a = [[a]]

--a)
dimOK :: Mat a -> Bool
dimOK [] = True
dimOK (h:t) = all (\y -> length y == length h) t

--b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat l = (length l, length (head l))

--c)
addMat :: Num a => Mat a -> Mat a -> Mat a 
addMat = zipWith (zipWith (+))