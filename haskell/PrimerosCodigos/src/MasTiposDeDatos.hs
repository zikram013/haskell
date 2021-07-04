
module MasTiposDeDatos where
import Data.Char
import Data.List

{-1.Definición del tipo de datos Figura y función que calcula el perímetro de una
figura.-}
type Radio = Float
type Lado = Float

data Figura = Circulo Radio | Cuadrado Lado | Rectangulo Lado Lado  deriving Show

perimetro::Figura->Float
perimetro (Circulo radio)= 2 * pi * radio
perimetro (Cuadrado lado) = 4 * lado
perimetro (Rectangulo ancho alto) = 2 * ancho + 2 * alto

{-2.Definición de tipos Real, Imaginario, Complejo y Resultado. El tipo Resultado 
almacena las soluciones de una ecuacion de segundo grado.
La función 'raices' calcula las raíces de una ecuación de segundo grado de
la forma: ax^2 + bx + c = 0-}
type Real = Float
type Imag = Float

data Compleo = Float :-Float deriving Show
data Resultado = UnaReal Float | DorReales Float Float | DosComplejas Float Float deriving Show

{-
Ejercicio 6.4
Dado el tipo recursivo Nat, algunas operaciones sobre
dicho tipo.
-}

data Nat = Cero | Suc Nat deriving Show

cinco :: Nat
cinco = Suc(Suc(Suc(Suc(Suc Cero))))

dos :: Nat
dos = Suc(Suc Cero)

uno :: Nat
uno = Suc Cero

-- resta
resta :: Nat -> Nat -> Nat
resta Cero _ = Cero
resta x Cero = x
resta (Suc x) (Suc y) = resta x y

{- Funciones de plegado, más abajo
-- suma
suma :: Nat -> Nat -> Nat
suma x Cero = x
suma x (Suc y) = suma (Suc x) y
-- producto
producto :: Nat -> Nat -> Nat
producto _ Cero = Cero
producto x (Suc y) = suma x (producto x y)
-}

-- potencia
potencia :: Nat -> Nat -> Nat
potencia _ Cero = Suc Cero
potencia Cero _ = Cero
potencia x (Suc Cero) = x
potencia x (Suc y) = producto x (potencia x y)

-- convierte un número de Integer a Nat

intANat :: Integer -> Nat
intANat 0 = Cero
intANat (n + 1) = Suc(intANat(n))

-- convierte de Nat a Integer

natAInt :: Nat -> Integer
natAInt Cero = 0
natAInt (Suc x) = 1 + natAInt x

-- cociente y resto de dividir dos Nat

resto :: Integer -> Integer -> Integer
resto x y
    |x < y = x
    |otherwise = resto (x - y) y

cociente :: Integer -> Integer -> Integer
cociente x y
    |x < y = 0
    |otherwise = 1 + cociente (x - y) y

cocNat :: Nat -> Nat -> Nat
cocNat x y = intANat(cociente (natAInt x) (natAInt y))

resNat :: Nat -> Nat -> Nat
resNat x y = intANat(resto (natAInt x) (natAInt y))

{-
Ejercicio 6.5
Definir las funciones suma y productoS con la función de plegado
foldNat.
-}

foldNat :: (a -> a) -> a -> (Nat -> a)
foldNat f e Cero = e
foldNat f e (Suc n) = f(foldNat f e n)

{-
suma :: Nat -> Nat -> Nat
suma x Cero = x
suma x (Suc y) = Suc(suma x y)
-}

suma :: Nat -> Nat -> Nat
suma m = foldNat Suc m

{-
producto :: Nat -> Nat -> Nat
producto _ Cero = Cero
producto x (Suc y) = suma x (producto x y)
-}

producto :: Nat -> Nat -> Nat
producto n = foldNat (\x -> suma x n) Cero

{-
Ejercicio 7.1
Tipo para representar números racionales.
-}

infix 9 :/
data Racional = Integer :/ Integer

-- La desigualdad se infiere a partir de ==
instance Eq Racional where
	(a :/ b) == (c :/ d) = (a * d) == (b * c)

-- Implementar compare o (<=) es suficiente
instance Ord Racional where
	compare (a :/ b) (c :/ d)
		| (a :/ b) == (c :/ d) = EQ
		| (a * c) <= (b * d) = LT
		| otherwise = GT
		
-- Aprovechamos que Integer ya es instancia de Show
instance Show Racional where
	show (a :/ b) = show a ++  "/" ++ show b

-- Operaciones de suma, resta y multiplicación	
instance Num Racional where
	(+) (a :/ b) (c :/ d) = (((a * d) + (c * b)) :/ (b * d))
	(-) (a :/ b) (c :/ d) = (((a * d) - (c * b)) :/ (b * d))
	(*) (a :/ b) (c :/ d) = ((a * c) :/ (b * d))
	abs (a :/ b) = ((abs a) :/ (abs b))
	signum (a :/ b) = (a :/ b) -- ?
	fromInteger x = (x :/ 1)
	
instance Fractional Racional where
	(/) (a :/ b) (c :/ d) = ((a * d) :/ (b * c))
	fromRational a = (1 :/ 2) -- ¿?
	
{-
Ejercicio 7.2
Hacer el tipo 'Nat' (definido más arriba) instancia de Eq, Ord y Num.
-}

instance Eq Nat where
	Cero == Cero = True
	(Suc x) == (Suc y) = x == y
	_ == _ = False

instance Ord Nat where
	compare Cero Cero = EQ
	compare Cero (Suc y) = LT
	compare (Suc x) Cero = GT
	compare (Suc x) (Suc y) = compare x y
	
instance Num Nat where
	(+) a b = suma a b
	(-) a b = resta a b
	(*) a b = producto a b
	abs a = a
	signum a = a -- ?
	fromInteger x = intANat x