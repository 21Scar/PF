data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

posx::Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = cos a 

posy::Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = sin a 

data Figura = Circulo Ponto Double | Retangulo Ponto Ponto | Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligno::Figura -> Bool
poligno (Circulo c r) = False
poligno (Retangulo p1 p2) = (posx p1 /= posx p2) || (posy p1 /= posy p2)
poligno (Triangulo p1 p2 p3) = ((posy p2 - posy p1) / (posx p2 - posx p1)) /= ((posy p3 - posy p2) / (posx p3 - posx p2))

vertices::Figura -> [Ponto]
vertices (Circulo c r)=[]
vertices (Retangulo p1 p2)=[p1,Cartesiano (posx p1)(posy p2),Cartesiano (posx p2)(posy p1),p2]
vertices (Triangulo p1 p2 p3)= [p1,p2,p3] 