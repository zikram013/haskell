module EstudioExamen where
import Data.Char
--conversor
conversion ::String->Int
conversion = foldr(\ x y -> (digitToInt)x + 2*y ) 0 

bin2intR :: String -> Int
bin2intR "0" = 0
bin2intR (x:xs) = (digitToInt)x + 2 * (bin2intR xs)

sumaDigitosC :: String -> Int
sumaDigitosC xs = sum [digitToInt x | x <- xs, isDigit x]

sumaDigitosR :: String -> Int
sumaDigitosR [] = 0
sumaDigitosR (x:xs)
	| isDigit x = digitToInt x + sumaDigitosR xs
	| otherwise = sumaDigitosR xs

int2bin :: Int -> [Int]
int2bin n | n < 2     = [n]
          | otherwise = n `mod` 2 : int2bin (n `div` 2)

--Divisores
divisores::IO()
divisores=do
	putStrLn ("Dime el nombre del archivo")
	fichero<-getLine
	putStrLn ("Dime un numero para darte sus divisores")
	numero <-getLine
	let d = ((read numero)::Int)
	if d == 0 then
		putStrLn("adios")
		else
		appendFile fichero (factores d)
		


		
factores :: Int ->String
factores n =unwords (map show [x|x <- [1..n], n `mod` x == 0 ])


{-Haskell abril 2021-}
data Categoria = ATP1000 | ATP500 | ATP250 | GrandSlam deriving Show
type Nombre = String
data Torneo = Tor Nombre Categoria
data Temporada = Temp [Torneo]

openAustralia :: Torneo
openAustralia = Tor "Open de Australia" GrandSlam
indianWells :: Torneo
indianWells = Tor "Indian Wells" ATP1000
mutuaMadridOpen :: Torneo
mutuaMadridOpen = Tor "Mutua Madrid Open" ATP1000
wimbledon :: Torneo
wimbledon = Tor "Wimbledon" GrandSlam
temporada2013 :: Temporada
temporada2013 = Temp [openAustralia, indianWells, mutuaMadridOpen, wimbledon]
{-
Se pide implementar una función utilizando funciones de plegado que dada una temporada sea capaz de
mostrar el listado de torneos de GrandSlam que tiene esa temporada. Un ejemplo de aplicación de la
función junto con la salida que debe obtener es el siguiente:
-}
getNombre::Torneo->String
getNombre (Tor n _) = n

getCategoria::Torneo->Categoria
getCategoria (Tor _ n) = n

instance Show Torneo where
	show torneo = "Torneo:"++getNombre(torneo) ++ "CAtegoria:" ++ show (getCategoria(torneo)) ++ "\n"
	

--torneoPorCategoria::Temporada->[Torneo]
--torneoPorCategoria (Temp lista) = foldr (\t ac -> if (getCategoria(t)==GrandSlam) then ac++[t] else ac) [] lista

{-
Dada la siguiente definición de datos que representa una lista de elementos
¿Cómo se representaría con este tipo de datos la lista [1, 2, 3]?
Implementa una función para calcular la longitud de una lista representada con el tipo
de datos List
-}
data List a = Cons a (List a) | Nil
--Cons 1(Cons 2 (Cons 3 NIL)) -- Para representar una lista

longitud::List a -> Int
longitud Nil = 0
longitud (Cons a lista) = 1 + longitud lista

{-
La Agencia de Seguridad Aérea desea crear un programa en Haskell que les permita gestionar sucesos de
seguridad aérea. Los sucesos se clasifican en dos categorías de acuerdo con su gravedad, accidentes e
incidentes, cuya diferencia estriba en que en los accidentes se producen víctimas mortales, mientras que
en los incidentes no. Además de esta diferencia, en ambos casos interesa almacenar la siguiente
información:
Aeropuerto: el nombre del aeropuerto donde se produjo el suceso
Heridos leves: el número de heridos que no requieren hospitalización
--Definir el tipo de dato adecuado para gestionar sucesos.
--Implementar todo lo necesario para poder comparar dos sucesos con la función ==.
Dos sucesos serán iguales si son del mismo tipo (accidente o incidente) y si todos sus campos son
iguales.
-}
data Suceso = Accidente String Int Int Int | Incidente String Int Int
instance Eq Suceso where
	Accidente a m l g == Accidente a1 m1 l1 g1 = a==a1 && m==m1 && l==l1 && g==g1
	Incidente a l g == Incidente a1 l1 g1 = a==a1 && l==l1 && g==g1
	_ == _ = False
	
{-
Se pide implementar una función polimórfica en Haskell que reciba un árbol binario y devuelva dos
listas: una con los nodos sin repetir y otra con los nodos que están repetidos. En ambas listas habrá
que incluir tanto los nodos hoja (nodos sin descendientes) como los nodos interiores (nodos que no
son hojas). Ejemplos de aplicación de la función podrían ser:
> separarNodos (Rama (Rama (Rama AV 12 AV) 49 (Rama (Rama AV 23 AV) 12 (Rama AV 13 AV)))
13 (Rama AV 10 AV))
([49,23,10],[12,13])
> separarNodos (Rama (Rama (Rama (Rama AV 'D' AV) '1' (Rama AV 'R' AV)) 'D' (Rama (Rama
AV 'f' AV) '7' (Rama AV '1' AV))) 'R' (Rama AV 'h' AV))
("f7h","D1R")
-}
data Arbol a = AV | Rama (Arbol a) a (Arbol a)

arbolAlista::Arbol a ->[a]
arbolAlista AV = []
arbolAlista (Rama i n d) = arbolAlista i ++ [n] ++ arbolAlista d

contarRec::Eq a=>[a]->[(a,Int)]
contarRec [] = []
contarRec l@(x:xs) = (x,freq) : contarRec l_sin_x 
	where freq = length (filter(==x)l)
	      l_sin_x = filter (/= x) xs
	      
partirLista::Eq a=>[a]->([a],[a])
partirLista [] = ([],[])
partirLista xs = (unicos,repetidos) 
	where unicos = [x |(x,y)<-contarRec xs , y==1]
	      repetidos = [x|(x,y)<-contarRec xs, y>1]

separarNodos::Eq a => Arbol a->([a],[a])
separarNodos = partirLista.arbolAlista