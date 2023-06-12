module Ficha8 where

data Frac = F Integer Integer

--a)
mdc :: Integer -> Integer -> Integer
mdc x y | x==y = x
        | x > y = mdc (x-y) y
        | otherwise = mdc x(y-x)

normaliza :: Frac -> Frac 
normaliza (F x y) = F (s*a) b

    where d = mdc (abs x) (abs y) 
          a = div (abs x) d
          b = div (abs y) d
          s = (signum x)*(signum y)

--abs dá o módulo

--b)
instance Eq Frac where
    f1 == f2 = ( a==x ) && ( b==y )
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

--c)
instance Ord Frac where
    f1 <= f2 = a*y <= x*b
        where (F a b) = normaliza f1
              (F x y) = normaliza f2

--d)
instance Show Frac where
   --show :: Frac -> String
   show (F a b) = "("++(show a)++ "/"++(show b)++")"

--e)
instance Num Frac where
    (F a b) + (F x y) = F (a*y +b*x) (b*y)
    (F a b) * (F x y) = F (a*x) (b*y)
    negate (F a b) = F (-a) b 
    abs (F a b) = F (abs a) (abs b)
    signum (F a b) = F((signum a)*(signum b)) 1
    fromInteger n = F n 1     

--f)
maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f l = filter (>(2*f)) l

--2.
data ExpInt = Const Int
             | Simetrico (Exp a)
             | Mais (Exp a) (Exp a)
             | Menos (Exp a) (Exp a)
             | Mult (Exp a) (Exp a)

exp1 = (Mais (Const 3) (Menos (Const 2) (Const 5)))

instance (Show a) => Show (Exp a) where
    show (Const x) = show x