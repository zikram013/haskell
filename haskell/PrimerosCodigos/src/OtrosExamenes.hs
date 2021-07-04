
module OtrosExamenes where
{-
Ejercicio 2
-}

-- a) Dada una lista, devuelve un par formado por el mayor prefijo ordenado
--	ascendentemente y el resto de la lista

ultimoOrdenado :: Ord a => [a] -> Int
ultimoOrdenado [] = 0
ultimoOrdenado [x] = 1
ultimoOrdenado (x:y:ys)
	| x <= y = 1 + ultimoOrdenado (y:ys)
	| otherwise = 1

mayorPrefijo :: Ord a => [a] -> ([a],[a])
mayorPrefijo xs = splitAt (ultimoOrdenado xs) xs

-- b) Dada una lista, devuelve el segmento más largo ordenado ascendentemente 
--	que aparezca en dicha lista.

segmentosOrdenados :: Ord a => [a] -> [[a]]
segmentosOrdenados [] = [[]]
segmentosOrdenados (x:xs) = (\(x,y) -> x) (mayorPrefijo (x:xs)) : segmentosOrdenados xs

masLargo :: [[a]] -> [a]
masLargo [] = []
masLargo [x] = x
masLargo (x:y:ys)
	| (length x) > (length y) = masLargo (x:ys)
	| otherwise = masLargo (y:ys)

mayorSegmento :: Ord a => [a] -> [a]
mayorSegmento xs = masLargo (segmentosOrdenados xs)

{- c) Sea la siguiente función 'scanl' predefinida en Prelude de Haskell:
	scanl :: (a -> b -> a) -> a -> [b] -> a
	scanl f y [] = [y]
	scanl f y (x:xs) = y : scanl f (f y x) xs
	define a partir de 'scanl' una función no recursiva 'facts' que dado
	un número n, devuelva una lista con los factoriales desde 0! hasta n!
-}

factorial :: Integer -> Integer
factorial 0 = 1
factorial n@(m + 1) = n * factorial m

facts :: Integer -> [Integer]
facts 0 = [1]
facts n = scanl (\y x -> factorial x)  (factorial 0) [1..n]

{-
Problema 2
-}

-- Dada la siguiente definición de tipo

-- data Maybe a = Nothing | Just a deriving Show

{- y la siguiente función que empareja los elementos de dos listas
zip :: [a] -> [b] -> [(a,b)]
zip(x:xs)(y:ys) = (x,y) : zip xs ys
zip _ _ = []
-}

-- a) Definir una función 'primeraPosicion' que devuelva la primera
--		posición que ocupa un elemento en la lista. Nothing si no está.

primeraPosicion :: Eq a => a -> [a] -> Maybe Int
primeraPosicion x ls = pos x (zip ls [0..])
	where
		pos _ [] = Nothing
		pos x ((y,n):ys)
			| x == y = Just n
			| otherwise = pos x ys
			
-- b) Expresar la función usando 'foldl'
{-
foldl f z []     = z       
foldl f z (x:xs) = foldl f (f z x) xs 
-}

prPosicion :: Eq a => a -> [a] -> Maybe Int
prPosicion x ls = foldl fb Nothing (zip ls [0..])
	where
		fb (Just q) _ = Just q
		fb Nothing (y,n)
			| x == y = Just n
			| otherwise = Nothing
			
-- c) Definir la función 'posiciones' que, dado un elemento y una lista,
--		devuelva una lista con todas las posiciones que ocupa dicho elemento
--		en la lista.

posiciones :: Eq a => a -> [a] -> [Int]
posiciones x ls = foldl fb [] (zip ls [0..])
	where
		fb ys (y,n)
			| x == y = ys ++ [n]
			| otherwise = ys
			
-- d) Expresar la función anterior usando una lista por compresión y la
--		función 'zip'

posiciones2 :: Eq a => a -> [a] -> [Int]
posiciones2 x ls = [n | (y, n) <- (zip ls [0..]), x == y]

{-
Problema 3
-}

-- Dado el siguiente tipo para representar árboles no vacíos

data Arbol23 a = Hoja a
				| Nodo2 a (Arbol23 a) (Arbol23 a)
				| Nodo3 a (Arbol23 a) (Arbol23 a) (Arbol23 a) deriving Show
				
-- a) Definir la función 'espejo', que devuelve la imagen especular
--		de un árbol dado.

espejo :: Arbol23 a -> Arbol23 a
espejo (Hoja x) = Hoja x
espejo (Nodo2 x i d) = Nodo2 x (espejo d) (espejo i)
espejo (Nodo3 x i c d) = Nodo3 x (espejo d) (espejo c) (espejo i)

-- b) Definir la función 'pertenece', que comprueba si un elemento
--		pertenece al árbol.

pertenece :: Eq a => a -> Arbol23 a -> Bool
pertenece e (Hoja x) = e == x
pertenece e (Nodo2 x i d) = (e == x) || (pertenece e i) || (pertenece e d)
pertenece e (Nodo3 x i c d) = (e == x) || (pertenece e i) || (pertenece e c) || (pertenece e d)

-- c) Definir la función de plegado 'foldA23' para este tipo de árboles.

foldA23 :: (a -> b -> b -> b -> b) -> (a -> b -> b -> b) -> (a -> b) -> Arbol23 a -> b
foldA23 f g h = fun
	where
		fun (Hoja x) = h x
		fun (Nodo2 x i d) = g x (fun i) (fun d)
		fun (Nodo3 x i c d) = f x (fun i) (fun c) (fun d)
		
-- d) Expresar las funciones 'espejo' y 'pertenece' en función de 'foldA23'.

espejofoldA23 :: Arbol23 a -> Arbol23 a
espejofoldA23 = foldA23 (\r i c d -> Nodo3 r d c i) (\r i d -> Nodo2 r d i) Hoja

pertenecefoldA23 :: Eq a => a -> Arbol23 a -> Bool
pertenecefoldA23 x = foldA23 (\r i c d -> (x == r) || i || c || d) (\r i d -> (x == r) || i || d) (x ==)

{-
Problema 2
Dada la función:
	
	f p g e [] = []
	f p g e (x:xs)
		| p (g x e) = (x, g x e) : f p g e xs
		| otherwise = f p g e xs
-}

f :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a, c)]
f2 :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a,c)]
f3 :: (c -> Bool) -> (a -> b -> c) -> b -> [a] -> [(a,c)]

-- a) Redefinición utilizando listas por comprensión

f p g e xs = [(x, y) | x <- xs, let y = g x e, p y] 

-- b) Refefinición utilizando 'filter' y 'map'

f2 p g e xs = map (\x -> (x, g x e)) (filter (\x -> p (g x e)) xs)

-- f2 = map (\x -> (x, g x e)) . filter (p . (flip g e))

-- c) Redefinición utilizando 'foldr'

f3 p g e = foldr fb []
	where
	 fb y ys
	 	| p (g y e) = (y, g y e) : ys
		| otherwise = ys

{-
Problema 3
Define una función 'comprime' cuyo comportamiento sea el siguiente:
	comprime f g [x1..xn] = f x1(f x2(...(f xn-1 (g xn))))
	comprime f g [x] = g x
-}

comprime :: (a -> b -> b) -> (a -> b) -> [a] -> b
comprime f g [x] = g x
comprime f g (x:xs) = f x (comprime f g xs)

{-
Problema 4
Define una función 'acumula' que reciba un dato y una lista ordenada de pares
donde la primera componente representa un dato y la segunda un contador y añada ese dato en la lista. Se considera que la lista está ordenada por la primera componente.
-}

acumula :: Ord a => a -> [(a, Integer)] -> [(a, Integer)]
acumula x [] = [(x, 1)]
acumula x ((y,n):ys)
	| x > y = (y,n) : acumula x ys
	| x == y = (y, n + 1) : ys
	| otherwise = (x, 1) : ((y,n):ys)


{-
Problema 5
Define, usando 'foldr', una función 'acumulado' que tome una lista y devuelva una lista ordenada de pares donde cada par representa el elemento y el número de apariciones.
-}

acumulado :: Ord a => [a] -> [(a, Integer)]
acumulado = foldr acumula []

{-
Problema 6
Operaciones sobre Árboles binarios utilizando plegados.
-}

data ArbolB a = Vacio | NodoB (ArbolB a) a (ArbolB a) deriving Show

foldB :: (a -> b -> b -> b) -> b -> ArbolB a -> b
foldB f z Vacio = z
foldB f z (NodoB i r d) = f r (foldB f z i) (foldB f z d)

-- 'apariciones', dado un valor del tipo a y un árbol, calcula el número de vece-- que aparece el valor dado en el árbol.

apariciones :: Eq a => a -> ArbolB a -> Int
apariciones e = foldB fb 0
	where
		fb r soli sold
			| e == r = 1 + soli + sold
			| otherwise = soli + sold
			
			
{-
Ejercicio 2
-}

-- Dadas las siguientes definiciones de tipos

type Codigo = Integer
type Unidades = Integer
type Nombre = String
type PrecioUnitario = Double
type Producto = (Codigo, Nombre, PrecioUnitario)
type BD = [Producto]
type Compra = [Codigo]
type Albaran = [(Unidades, Nombre, PrecioUnitario)]

-- definir una funci?n 'mkAlbaran' que, dada una compra en la que pueden 
-- aparecer c?digos de productos repetidos en cualquier orden, y una base
-- de datos con las caracter?sticas de cada producto produzca el correspondiente-- albar?n.
{-
mkAlbaran :: BD -> Compra -> Albaran
mkAlbaran bd cs = mkAlb bd (agrupar cs)
mkAlb bd [] = []
mkAlb bd ((c,n):cns) = (n, nom, pr) : mkAlb bd cns
	where
		(_, nom, pr) = producto c bd
producto :: Codigo -> BD -> Producto
producto x [] = error "producto desconocido"
producto x ((c, n, p):ls) = if x == c then (c, n, p) else producto x ls
-- expresar 'mkAlbaran' con ayuda de foldr
mkAlbaran bd cs = foldr f [] (agrupar cs)
		where 
			f (c, n) rs = (n, nom, pr) : rs
				where (_, nom, pr) = producto c bd
-}
{-
Ejercicio 3
-}

-- Dada la siguiente definición de tipo

data Problema a = Cal a | Sub [Problema a] deriving Show

-- utilizado para representar los puntos correspondientes a los distintos
-- apartados de un problema de examen.

-- a) Define la función de plegado para el tipo Problema a

foldProblema :: ([b] -> b) -> (a -> b) -> Problema a -> b
foldProblema f g (Cal x) = g x
foldProblema f g (Sub xs) = f (map (foldProblema f g) xs)

-- b) Usando la función 'foldProblema', define una función 'notaTotal' que
--		calcule la puntuación total de un examen.

notaTotal :: Num a => Problema a -> a
notaTotal = foldProblema sum (id)

-- c) Usando la función 'foldProblema', define una función 'maximaNota' que
--		devuelva la nota con mayor valoración de un exámen.

maximaNota :: (Num a, Ord a) => Problema a -> a
maximaNota = foldProblema maximum (id)

