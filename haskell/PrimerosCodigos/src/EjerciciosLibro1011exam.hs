
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

{-Resolucion de problemas matematicos-}
{-16.Definir la funci�n
-- ullman :: (Num a, Ord a) => a -> Int -> [a] -> Bool
-- tal que (ullman t k xs) se verifica si xs tiene un subconjunto con k
-- elementos cuya suma sea menor que t. Por ejemplo,
-- ullman 9 3 [1..10] == True
-- ullman 5 3 [1..10] == False-}
ullman::(Ord a , Num a) => a->Int->[a]->Bool
ullman t k xs = sum (take k(sort xs))<t

{-17.Definir la funci�n
-- sumasDe2Cuadrados :: Integer -> [(Integer, Integer)]
-- tal que (sumasDe2Cuadrados n) es la lista de los pares de n�meros
-- tales que la suma de sus cuadrados es n y el primer elemento del par
-- es mayor o igual que el segundo. Por ejemplo,
-- sumasDe2Cuadrados 25 == [(5,0),(4,3)]-}
sumaDe2Cuadrados::Int -> [(Int,Int)]
sumaDe2Cuadrados n = [(x,y)|x<-[n,n-1..0], y<-[0..x],x*x+y*y==n]

{-18.(Basado en el problema 145 del Proyecto Euler). Se dice
-- que un n�mero n es reversible si su �ltima cifra es distinta de 0 y
-- la suma de n y el n�mero obtenido escribiendo las cifras de n en
-- orden inverso es un n�mero que tiene todas sus cifras impares. Por
-- ejemplo, 36 es reversible porque 36+63=99 tiene todas sus cifras
-- impares, 409 es reversible porque 409+904=1313 tiene todas sus cifras
-- impares, 243 no es reversible porque 243+342=585 no tiene todas sus
-- cifras impares.
-- Definir la funci�n
-- reversiblesMenores :: Int -> Int
-- tal que (reversiblesMenores n) es la cantidad de n�meros reversibles
-- menores que n. Por ejemplo,
-- reversiblesMenores 10 == 0
-- reversiblesMenores 100 == 20
-- reversiblesMenores 1000 == 120-}
reversibleMenores::Int->Int
reversibleMenores n = length [x | x <-[1..n-1],esReversible x]

esReversible::Int->Bool
esReversible n = rem n 10/=0 && impares (cifras(n+(inverso n)))

impares::[Int]->Bool
impares xs = and [odd x | x<-xs]

inverso::Int->Int
inverso n = read (reverse (show n))

cifras::Int->[Int]
cifras n = [read [x] | x<-show n]

{-19.Definir, usando funciones de orden superior, la funci�n
-- grafoReducido :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [(a,b)]
-- tal que (grafoReducido f p xs) es la lista (sin repeticiones) de los
-- pares formados por los elementos de xs que verifican el predicado p
-- y sus im�genes. Por ejemplo,
-- grafoReducido (^2) even [1..9] == [(2,4),(4,16),(6,36),(8,64)]
-- grafoReducido (+4) even (replicate 40 1) == []
-- grafoReducido (*5) even (replicate 40 2) == [(2,10)]-}
grafoReducido :: Eq a => (a -> b) -> (a -> Bool) -> [a] -> [(a,b)]
grafoReducido f p xs = [(x,f x) | x <- nub xs, p x]

{-20.Un n�mero natural n se denomina semiperfecto si es la
-- suma de algunos de sus divisores propios. Por ejemplo, 18 es
-- semiperfecto ya que sus divisores son 1, 2, 3, 6, 9 y se cumple que
-- 3+6+9=18.
--
-- Definir la funci�n
-- esSemiPerfecto :: Int -> Bool118 Ejercicios de programaci�n funcional (2011�12)
-- tal que (esSemiPerfecto n) se verifica si n es semiperfecto. Por
-- ejemplo,
-- esSemiPerfecto 18 == True
-- esSemiPerfecto 9 == False
-- esSemiPerfecto 24 == True-}
esSemiPerfecto :: Int->Bool
esSemiPerfecto n = or [sum ys == n | ys <-subconjuntos (divisores n)]

subconjuntos :: [a] -> [[a]]
subconjuntos [] = [[]]
subconjuntos (x:xs) = zss++[x:ys | ys <- zss]
	where zss = subconjuntos xs

divisores :: Int->[Int]
divisores n = [x | x<-[1..n-1],mod n x == 0]

{-21.Definir mediante plegado la funci�n
-- producto :: Num a => [a] -> a
-- tal que (producto xs) es el producto de los elementos de la lista
-- xs. Por ejemplo,
-- producto [2,1,-3,4,5,-6] == 720-}
producto::Num a=>[a]->a
producto = foldr (*) 1

{-22.Definir mediante plegado la funci�n
-- productoPred :: Num a => (a -> Bool) -> [a] -> a
-- tal que (productoPred p xs) es el producto de los elementos de la
-- lista xs que verifican el predicado p. Por ejemplo,
-- productoPred even [2,1,-3,4,-5,6] == 48-}
productoPredicado::Num a =>(a->Bool)->[a]->a
productoPredicado p = foldr (\x y ->if p x then x*y else y)1

{-23.Definir la funci�n
-- productoPos :: (Num a, Ord a) => [a] -> a
-- tal que (productoPos xs) esel producto de los elementos estr�ctamente
-- positivos de la lista xs. Por ejemplo,
-- productoPos [2,1,-3,4,-5,6] == 48-}
productoPositivo::(Num a,Ord a)=>[a]->a
productoPositivo = productoPredicado (>0)

{-24.Las relaciones finitas se pueden representar mediante120 Ejercicios de programaci�n funcional (2011�12)
-- listas de pares. Por ejemplo,
-- r1, r2, r3 :: [(Int, Int)]
-- r1 = [(1,3), (2,6), (8,9), (2,7)]
-- r2 = [(1,3), (2,6), (8,9), (3,7)]
-- r3 = [(1,3), (2,6), (8,9), (3,6)]
-- Definir la funci�n
-- esFuncion :: (Eq a, Eq b) => [(a,b)] -> Bool
-- tal que (esFuncion r) se verifica si la relaci�n r es una funci�n (es
-- decir, a cada elemento del dominio de la relaci�n r le corresponde un
-- �nico elemento). Por ejemplo,
-- esFuncion r1 == False
-- esFuncion r2 == True
-- esFuncion r3 == True-}
r1, r2, r3 :: [(Int, Int)]
r1 = [(1,3), (2,6), (8,9), (2,7)]
r2 = [(1,3), (2,6), (8,9), (3,7)]
r3 = [(1,3), (2,6), (8,9), (3,6)]
esFuncion :: (Eq a, Eq b) => [(a,b)] -> Bool
esFuncion [] = True
esFuncion ((x,y):r) = [y1 | (x1,y1) <- r, x == x1, y /= y1] == [] && esFuncion r

{-25.Se denomina cola de una lista l a una sublista no
-- vac�a de l formada por un elemento y los siguientes hasta el
-- final. Por ejemplo, [3,4,5] es una cola de la lista [1,2,3,4,5].
--
-- Definir la funci�n
-- colas :: [a] -> [[a]]
-- tal que (colas xs) es la lista de las colas
-- de la lista xs. Por ejemplo,
-- colas [] == [[]]
-- colas [1,2] == [[1,2],[2],[]]
-- colas [4,1,2,5] == [[4,1,2,5],[1,2,5],[2,5],[5],[]]-}
colas :: [a] -> [[a]]
colas [] = [[]]
colas (x:xs) = (x:xs) : colas xs

{-26. Se denomina cabeza de una lista l a una sublista no
-- vac�a de la formada por el primer elemento y los siguientes hasta uno
-- dado. Por ejemplo, [1,2,3] es una cabeza de [1,2,3,4,5].
--
-- Definir la funci�n
-- cabezas :: [a] -> [[a]]
-- tal que (cabezas xs) es la lista de las cabezas de la lista xs. Por
-- ejemplo,
-- cabezas [] == [[]]
-- cabezas [1,4] == [[],[1],[1,4]]
-- cabezas [1,4,5,2,3] == [[],[1],[1,4],[1,4,5],[1,4,5,2],[1,4,5,2,3]]-}
cabezasRecursion::[a]->[[a]]
cabezasRecursion [] = [[]]
cabezasRecursion (x:xs) = [] : [x:ys | ys<-cabezasRecursion xs]

cabezasPlegado::[a]->[[a]]
cabezasPlegado = foldr (\x y ->[x]:[x:ys | ys<-y]) []

{-27.[La identidad de Bezout] Definir la funci�n
-- bezout :: Integer -> Integer -> (Integer, Integer)
-- tal que (bezout a b) es un par de n�meros x e y tal que a*x+b*y es el
-- m�ximo com�n divisor de a y b. Por ejemplo,
-- bezout 21 15 == (-2,3)
-- Indicaci�n: Se puede usar la funci�n quotRem tal que (quotRem x y) es
-- el par formado por el cociente y el resto de dividir x entre y
 Sean q, r el cociente y el resto de a entre b, d el m�ximo com�n
-- denominador de a y b y (x,y) el valor de (bezout b r) . Entonces,
-- a = bp+r
-- d = bx+ry
-- Por tanto,
-- d = bx + (a-bp)y
-- = ay + b(x-qy)
-- Luego,
-- bezout a b = (y,x-qy)-}
bezout :: Int->Int->(Int,Int)
bezout _ 0 =(1,0)
bezout _ 1 =(0,1)
bezout a b = (y,x-q*y) 
	where (x,y) = bezout b r
	      (q,r)=qoutRem (a,b)
	  
qoutRem::(Int,Int)->(Int,Int)
qoutRem (a,b) = (a `mod` b , a`rem`b)


{-Relacion de propiedades por inducion-}

{-28.Definir por recursi�n la funci�n
-- sumaImpares :: Int -> Int
-- tal que (sumaImpares n) es la suma de los n primeros n�meros impares. Por ejemplo,
-- sumaImpares 5 == 25-}
sumaImpares::Int->Int
sumaImpares 0 = 0
sumaImpares (n+1)=sumaImpares n + (2*n+1)

{-29.Definir la funcion grafoSumaImpares :: Int -> Int -> [(Int,Int)]
-- tal que (grafoSumaImpares m n) es la lista formadas por los n�meros x
-- entre m y n y los valores de (sumaImpares x).
-- Calcular (grafoSumaImpares 1 9).-}
grafoSumaImpares::Int->Int->[(Int,Int)]
grafoSumaImpares m n = [(x,sumaImpares x)|x<-[m..n]]


{-30.Definir por recursi�n la funci�n
-- sumaPotenciasDeDosMasUno :: Int -> Int
-- tal que
-- sumaPotenciasDeDosMasUno n = 1 + 2^0 + 2^1 + 2^2 + ... + 2^n.
-- Por ejemplo,
-- sumaPotenciasDeDosMasUno 3 == 16-}
sumaPotenciasDeDosMasUno::Int->Int
sumaPotenciasDeDosMasUno 0 = 2
sumaPotenciasDeDosMasUno (n+1) = sumaPotenciasDeDosMasUno n + 2^(n+1)

{-31.Definir por recursi�n la funci�n
-- copia :: Int -> a -> [a]
-- tal que (copia n x) es la lista formado por n copias del elemento
-- x. Por ejemplo,
-- copia 3 2 == [2,2,2]-}
copia::Int->a->[a]
copia 0 _ = []
copia (n+1) x = x : copia n x

{-32.Definir la funci�n
-- traspuesta :: [[a]] -> [[a]]
-- tal que (traspuesta m) es la traspuesta de la matriz m. Por ejemplo,
-- traspuesta [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
-- traspuesta [[1,4],[2,5],[3,6]] == [[1,2,3],[4,5,6]]-}
traspuesta::[[a]]->[[a]]
traspuesta [] = []
traspuesta([]:xss)=traspuesta xss
traspuesta((x:xs):xss)=(x:[h | (h:_)<-xss]):traspuesta (xs:[t | (_:t)<-xss])

{-Ejercicios sobre numeros primos-}
{-33.Definir la funci�n mediante compresion,recurion y plegado
-- elimina :: Int -> [Int] -> [Int]
-- tal que (elimina n xs) es la lista obtenida eliminando en la lista xs
-- los m�ltiplos de n. Por ejemplo,
-- elimina 3 [2,3,8,9,5,6,7] == [2,8,5,7]-}
eliminaCompresion::Int->[Int]->[Int]
eliminaCompresion n xs = [x|x<-xs,x`mod`n /=0]

eliminaRecursion::Int->[Int]->[Int]
eliminaRecursion n [] = []
eliminaRecursion n (x:xs) 
	  | mod x n == 0 = eliminaRecursion n xs
	  | otherwise = x : eliminaRecursion n xs

eliminaPlegado::Int->[Int]->[Int]
eliminaPlegado n = foldr f []
	where f x y | mod x n == 0 = y
	             |otherwise = x:y
	             
{-34.Definir la funci�n
-- criba :: [Int] -> [Int]
-- tal que (criba xs) es la lista obtenida cribando la lista xs con el
-- m�todo descrito anteriormente. Por ejemplo,
-- criba [2..20] == [2,3,5,7,11,13,17,19]
-- take 10 (criba [2..]) == [2,3,5,7,11,13,17,19,23,29]-}
criba::[Int]->[Int]
criba [] = []
criba (n:ns) = n : criba(eliminaPlegado n ns)

{-35.Definir la funci�n
-- prefijosConSuma :: [Int] -> Int -> [[Int]]
-- tal que (prefijosConSuma xs n) es la lista de los prefijos de xs cuya
-- suma es n. Por ejemplo,
-- prefijosConSuma [1..10] 3 == [[1,2]]
-- prefijosConSuma [1..10] 4 == []-}
prefijosConSuma::[Int]->Int->[[Int]]
prefijosConSuma [] 0 = [[]]
prefijosConSuma [] n = []
prefijosConSuma (x:xs) n 
	|x<n = [x:ys | ys <-prefijosConSuma xs (n-x)]
	|x==n=[[x]]
	|x>n=[]
	
{-36.Definir la funci�n
-- consecutivosConSuma :: [Int] -> Int -> [[Int]]
-- (consecutivosConSuma xs n) es la lista de los elementos consecutivos
-- de xs cuya suma es n. Por ejemplo,
-- consecutivosConSuma [1..10] 9 == [[2,3,4],[4,5],[9]]-}
consecutivosConSuma::[Int]->Int->[[Int]]
consecutivosConSuma [] 0 = [[]]
consecutivosConSuma [] n = []
consecutivosConSuma (x:xs) n = (prefijosConSuma(x:xs)n)++(consecutivosConSuma xs n)

{-37.Definir la funci�n
-- sumaCifras :: Int -> Int
-- tal que (sumaCifras x) es la suma de las cifras del n�mero x. Por
-- ejemplo,
-- sumaCifras 254 == 11-}
sumaCifras::Int->Int
sumaCifras x = sum [read [y] | y<-show x]

{-38.Definir la funci�n por recursion y plegado y compresion
-- sumaCifrasLista :: [Int] -> Int
-- tal que (sumaCifrasLista xs) es la suma de las cifras de la lista de
-- n�meros xs. Por ejemplo,
-- sumaCifrasLista [254, 61] == 18-}
sumaCifrasListaCompresion::[Int]->Int
sumaCifrasListaCompresion xs = sum [sumaCifras y | y<-xs]

sumaCifrasListaRecursion :: [Int] -> Int
sumaCifrasListaRecursion [] = 0
sumaCifrasListaRecursion (x:xs) = sumaCifras x + sumaCifrasListaRecursion xs

sumaCifrasPlegado::[Int]->Int
sumaCifrasPlegado = foldr f 0 where f x y = sumaCifras x + y

{-38.Conjecura de collaz, siempre llegar al uno
Sea la siguiente operaci�n, aplicable a cualquier n�mero entero
-- positivo:
-- * Si el n�mero es par, se divide entre 2.
-- * Si el n�mero es impar, se multiplica por 3 y se suma 1.-}
siguiente::Int->Int
siguiente n | even n = n`div` 2
			|otherwise = 3*n+1
			
collatz::Int->[Int]
collatz 1 = [1]
collatz n = n: collatz (siguiente n)

{-Ejercicios de examenesdel curso 2010-11-}
