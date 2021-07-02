
module EjerciciosLibro1011exam where
import Data.Char
import Data.List

{-Definiciones sobre cadenas, orden superior y plegado-}

{-1.Una persona es tan agarrada que s�lo compra cuando le
-- hacen un descuento del 10% y el precio (con el descuento) es menor o
-- igual que 199.
--
-- Definir, usando comprensi�n, la funci�n
-- agarrado :: [Float] -> Float
-- tal que (agarrado ps) es el precio que tiene que pagar por una compra
-- cuya lista de precios es ps. Por ejemplo,
-- agarrado [45.00, 199.00, 220.00, 399.00] == 417.59998-}
agarrado::[Float]->Float
agarrado ps = sum [p * 0.9 | p<-ps, p*0.9<=199]
--Definirlo por recurision
agarradoRecursion::[Float]->Float
agarradoRecursion [] = 0
agarradoRecursion (p:ps) 
					|precioConDescuento <=199 = precioConDescuento + agarradoRecursion ps
					|otherwise = agarradoRecursion ps
						where precioConDescuento = p*0.9
						
{-2.La funci�n
-- divideMedia :: [Double] -> ([Double],[Double])
-- dada una lista num�rica, xs, calcula el par (ys,zs), donde ys
-- contiene los elementos de xs estrictamente menores que la media,
-- mientras que zs contiene los elementos de xs estrictamente mayores
-- que la media. Por ejemplo,
-- divideMedia [6,7,2,8,6,3,4] == ([2.0,3.0,4.0],[6.0,7.0,8.0,6.0])84 Ejercicios de programaci�n funcional (2011�12)
-- divideMedia [1,2,3] == ([1.0],[3.0])
-- Definir la funci�n divideMedia por filtrado, comprensi�n y
-- recursi�n.-}
media::[Double]->Double
media xs = (sum xs)/fromIntegral (length xs)
--filtrado
divideMediaFiltrado::[Double]->([Double],[Double])
divideMediaFiltrado xs = (filter(<m)xs,filter(>m)xs) where m = media xs
--Compresion
divideMediaCompresion::[Double]->([Double],[Double])
divideMediaCompresion xs = ([x | x<-xs,x<m],[x|x<-xs,x>m]) where m = media xs
--recursion
divideMediaRecursion::[Double]->([Double],[Double])
divideMediaRecursion xs = divideMediaAux xs 
	where m = media xs
	      divideMediaAux [] = ([],[])
	      divideMediaAux (x:xs) | x<m=(x:ys,zs)
	      						| x == m = (ys,zs)
	      						|x>m = (ys,x:zs)
	      							where (ys,zs) = divideMediaAux xs
	      							
{-3.Definir, por comprensi�n, la funci�n
-- sumaDigitos :: String -> Int
-- tal que (sumaDigitos xs) es la suma de los d�gitos de la cadena
-- xs. Por ejemplo,
-- sumaDigitos �SE 2431 X� == 10
-- Nota: Usar las funciones isDigit y digitToInt. Tambien por recursion
-}
sumaDigitos::String->Int
sumaDigitos xs = sum [digitToInt x | x<-xs, isDigit x]

sumaDigitosRecursion::String->Int
sumaDigitosRecursion [] = 0
sumaDigitosRecursion (x:xs) 
						|isDigit x = digitToInt x + sumaDigitosRecursion xs
						|otherwise = sumaDigitosRecursion xs
						
{-4. Definir, por comprensi�n y recursion la funci�n
-- mayusculaInicial :: String -> String
-- tal que (mayusculaInicial xs) es la palabra xs con la letra inicial
-- en may�scula y las restantes en min�sculas. Por ejemplo,
-- mayusculaInicial �sEviLLa� == �Sevilla�
-- Nota: Usar las funciones toLower y toUpper.-}
mayusculaInicial::String->String
mayusculaInicial [] = []
mayusculaInicial (x:xs)=toUpper x : [toLower x |x<-xs]

mayusculaInicialRecursivo::String->String
mayusculaInicialRecursivo [] = []
mayusculaInicialRecursivo (x:xs)=toUpper x : aux xs
	where aux(x:xs) = toLower x :aux xs
	      aux[]=[]
	      
{-5.Se consideran las siguientes reglas de may�sculas
-- iniciales para los t�tulos:
-- * la primera palabra comienza en may�scula y
-- * todas las palabras que tienen 4 letras como m�nimo empiezan
-- con may�sculas
-- Definir, por comprensi�n, la funci�n
-- titulo :: [String] -> [String]
-- tal que (titulo ps) es la lista de las palabras de ps con
-- las reglas de may�sculas iniciales de los t�tulos. Por ejemplo,
-- *Main> titulo [�eL�,�arTE�,�DE�,�La�,�proGraMacion�]
-- [�El�,�Arte�,�de�,�la�,�Programacion�]
Tambien de forma Recurisvo-}
titulo::[String]->[String]
titulo [] = []
titulo (p:ps) = mayusculaInicial p : [transforma p |p<-ps]

tituloRecursivo::[String]->[String]
tituloRecursivo [] = []
tituloRecursivo (p:ps)=mayusculaInicial p : tituloRecursivoAux ps
	where tituloRecursivoAux [] = []
	      tituloRecursivoAux (p:ps) = transforma p : tituloRecursivoAux ps
	
transforma::String->String
transforma p |length p>= 4 = mayusculaInicial p
			 | otherwise = minuscula p
			 
minuscula::String->String
minuscula xs = [toLower x | x<-xs]	      
{-6.Definir, por comprensi�n, la funci�n
-- buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
-- tal que (buscaCrucigrama l pos lon ps) es la lista de las palabras de
-- la lista de palabras ps que tienen longitud lon y poseen la letra l en
-- la posici�n pos (comenzando en 0). Por ejemplo,
-- *Main> buscaCrucigrama �c� 1 7 [�ocaso�, �casa�, �ocupado�]
-- [�ocupado�]
-- Tambien hacerlo recursivo-}
buscaCrucigrama::Char->Int->Int->[String]->[String]
buscaCrucigrama l pos lon ps = [p | p<-ps,length p == lon, 0<=pos, pos<length p, p !!pos==l]

buscaCrucigramaRecursivo::Char->Int->Int->[String]->[String]
buscaCrucigramaRecursivo letra pos lon [] = []
buscaCrucigramaRecursivo letra pos lon (p:ps) 
	|length p == lon &&0<=pos && pos<length p && p!!pos==letra = p : buscaCrucigramaRecursivo letra pos lon ps
	|otherwise = buscaCrucigramaRecursivo letra pos lon ps
	
{-7.Definir, por comprensi�ny recursion, la funci�n
-- posiciones :: String -> Char -> [Int]
-- tal que (posiciones xs y) es la lista de la posiciones del car�cter y
-- en la cadena xs. Por ejemplo,
-- posiciones �Salamamca� �a� == [1,3,5,8]-}
posiciones::String->Char->[Int]
posiciones xs y = [n |(x,n)<-zip xs [0..], x==y]

posicionesRecursivo::String->Char->[Int]
posicionesRecursivo xs y = posicionesRecursivoAux xs y 0 
	where posicionesRecursivoAux [] y n = []
	      posicionesRecursivoAux (x:xs) y n | x== y = n:posicionesRecursivoAux xs y (n+1)
	      									|otherwise = posicionesRecursivoAux xs y (n+1)
	      									
{-8.Definir, por recursi�n y compresion la funci�n
-- contieneRec :: String -> String -> Bool
-- tal que (contieneRec xs ys) se verifica si ys es una subcadena de
-- xs. Por ejemplo,
-- contieneRec �escasamente� �casa� == True
-- contieneRec �escasamente� �cante� == False
-- contieneRec �� �� == True
-- Nota: Se puede usar la predefinida (isPrefixOf ys xs) que se verifica
-- si ys es un prefijo de xs.-}
contieneRecursivo::String->String->Bool
contieneRecursivo _ [] = True
contieneRecursivo [] ys = False
contieneRecursivo xs ys = isPrefixOf ys xs || contieneRecursivo (tail xs)ys

contiene::String->String->Bool
contiene xs ys = sufijo xs ys /=[]

sufijo::String->String->[String]
sufijo xs ys = [x|x<-sufijos xs, isPrefixOf ys x]

sufijos::String->[String]
sufijos xs = [drop i xs | i <-[0..length xs]]

{-9. Se considera la funci�n
-- filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
-- tal que (filtraAplica f p xs) es la lista obtenida aplic�ndole a los
-- elementos de xs que cumplen el predicado p la funci�n f. Por ejemplo,
-- filtraAplica (4+) (<3) [1..7] => [5,6]
-- Se pide, definir la funci�n-}
-- 1. por comprensi�n,
filtraAplicaCompresion::(a->b)->(a->Bool)->[a]->[b]
filtraAplicaCompresion f p xs = [f x | x<-xs ,p x]
-- 2. por recursi�n 
filtraAplicaRecursion::(a->b)->(a->Bool)->[a]->[b]
filtraAplicaRecursion f p [] = []
filtraAplicaRecursion f p (x:xs) | p x = f x : filtraAplicaRecursion f p xs
								 |otherwise = filtraAplicaRecursion f p xs
-- 3. por plegado (con foldr
filtraAplicaPlegado::(a->b)->(a->Bool)->[a]->[b]
filtraAplicaPlegado f p = foldr(\x y -> if p x then (f x : y) else y)[]

{-10.Definir, mediante recursi�n, la funci�n
-- maximumR :: Ord a => [a] -> a
-- tal que (maximumR xs) es el m�ximo de la lista xs. Por ejemplo,
-- maximumR [3,7,2,5] == 7
-- Nota: La funci�n maximumR es equivalente a la predefinida maximum.-}
maximunRecursivo::Ord a =>[a]->a
maximunRecursivo [x] = x
maximunRecursivo (x:y:ys)=max x (maximunRecursivo (y:ys))

{-11.Definir, mediante recursi�n , recursion acumulador, plegado, la funci�nRelaci�n 10. Definiciones por plegado 101
-- inversaR :: [a] -> [a]
-- tal que (inversaR xs) es la inversa de la lista xs. Por ejemplo,
-- inversaR [3,5,2,4,7] == [7,4,2,5,3]-}
inversaRecursivo::[a]->[a]
inversaRecursivo [] = []
inversaRecursivo (x:xs) = (inversaRecursivo xs)++[x]

inversaPlegado::[a]->[a]
inversaPlegado = foldr f [] where f x y = y ++[x] --con foldl seria foldl (\ys x -> x:ys) []

inversaRecursionAcumulado::[a]->[a]
inversaRecursionAcumulado xs = inversaRecursivoAux [] xs
	where inversaRecursivoAux ys [] = ys
	      inversaRecursivoAux ys (x:xs) = inversaRecursivoAux (x:ys)xs
	      
{-12.Definir, por recursi�n con acumulador, plegado, la funci�n
-- dec2entR :: [Int] -> Int
-- tal que (dec2entR xs) es el entero correspondiente a la expresi�n
-- decimal xs. Por ejemplo,
-- dec2entR [2,3,4,5] == 2345-}
dec2entRecursivoAcumulado::[Int]->Int
dec2entRecursivoAcumulado xs = aux 0 xs
	where aux a [] = a
	      aux a (x:xs) = aux (10*a+x) xs

dec2entPlegado::[Int]->Int
dec2entPlegado = foldl (\a x -> 10*a+x)0

{-13.Definir, mediante recursi�n,plegado, la funci�n
-- sumllR :: Num a => [[a]] -> a
-- tal que (sumllR xss) es la suma de las sumas de las listas de xss.
-- Por ejemplo,
-- sumllR [[1,3],[2,5]] == 11-}
sumllRecursivo::Num a=>[[a]]->a
sumllRecursivo [] = 0
sumllRecursivo (xs:xss)=sum xs + sumllRecursivo xss

sumllPlegado::Num a=>[[a]]->a
sumllPlegado = foldr (\xs n -> sum xs + n)0

{-14.Definir, mediante recursi�n y plegado, la funci�n
-- borraR :: Eq a => a -> a -> [a]
-- tal que (borraR y xs) es la lista obtenida borrando las ocurrencias de
-- y en xs. Por ejemplo,
-- borraR 5 [2,3,5,6] == [2,3,6]
-- borraR 5 [2,3,5,6,5] == [2,3,6]
-- borraR 7 [2,3,5,6,5] == [2,3,5,6,5]-}
borrarRecursivo::Eq a =>a->[a]->[a]
borrarRecursivo z [] = []
borrarRecursivo z (x:xs) |z==x = borrarRecursivo z xs
						 |otherwise = x : borrarRecursivo z xs
				
borrarPlegado::Eq a=>a->[a]->[a]
borrarPlegado z = foldr (\x y -> if z ==x then y else x:y)[] 

{-15.Definir, mediante recursi�n plegado, la funci�n
-- diferenciaR :: Eq a => [a] -> [a] -> [a]
-- tal que (diferenciaR xs ys) es la diferencia del conjunto xs e ys; es
-- decir el conjunto de los elementos de xs que no pertenecen a ys. Por
-- ejemplo,diferenciaR [2,3,5,6] [5,2,7] == [3,6]-}
diferenciaRecursivo :: Eq a =>[a]->[a]->[a]
diferenciaRecursivo xs ys = auxDif xs xs ys
	where auxDif a xs [] = a
	      auxDif a xs (y:ys) = auxDif (borrarRecursivo y a)xs ys
	  
diferenciaPlegado::Eq a =>[a]->[a]->[a]
diferenciaPlegado xs ys = foldl (flip borrarRecursivo) xs ys