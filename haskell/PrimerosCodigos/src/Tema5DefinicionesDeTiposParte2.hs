
module Tema5DefinicionesDeTiposParte2 where

--El sistema de clases restringe el tipo de las funciones polimorficas
--al imponerles condiciones en su declaracion

pertenece :: (Eq a) => a-> [a]->Bool
pertenece x [] = False
pertenece x(y:ys) = (x==y) || pertenece x(ys)

--Derivacion automatica: utilizando la clausula deriving
data Color=Rojo|Amarillo|Azul|Verde deriving(Ord,Show)
--Declaracion de instancias: Se declara, para un tipo determinado, una instancia
-- de una clase concreta, redefiniendo las operaciones de esa clase para tipo
instance Eq Color where 
	Rojo==Rojo=True
	Amarillo==Amarillo=True
	Azul==Azul=True
	Verde==Verde=True
	_==_=False
--ejemplo
class Poligono p where
	area:: p -> Float
	perimetro:: p -> Float
type Base = Float
type Altura = Float
type Lado = Float
data Cuadrado = Cuadrado Lado deriving Show
data Rectangulo = Rectangulo Base Altura deriving Show
instance Poligono Cuadrado where
	area (Cuadrado lado) = lado*lado
	perimetro (Cuadrado lado) = 4*lado
instance Poligono Rectangulo where
	area (Rectangulo b a) = b * a
	perimetro (Rectangulo b a) = a*b + 2*a