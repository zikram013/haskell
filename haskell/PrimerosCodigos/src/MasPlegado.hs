
module MasPlegado where
import Data.Char
import Data.List

{-1.Redefinir, usando foldr, la funci�n filter. Por
-- ejemplo, 
--    filter' (<4) [1,7,3,2]  =>  [1,3,2]-}
filterPlegado::(a->Bool)->[a]->[a]
filterPlegado p = foldr (\x y -> if (p x)then(x:y) else y) []

{-2.Definir, usando foldr, la funci�n
--    inversaFR :: [a] -> [a]
-- tal que (inversaFR xs) es la inversa de la lista xs. Por ejemplo,
--    inversaFR [3,5,2,4,7]  =>  [7,4,2,5,3]-}
inversaPlegado::[a]->[a]
inversaPlegado = foldr (\x ys -> ys ++ [x]) []

{-3.Definir, usando foldl, la funci�n
--    inversaFL :: [a] -> [a]
-- tal que (inversaFL xs) es la inversa de la lista xs. Por ejemplo,
--    inversaFL [3,5,2,4,7]  ==  [7,4,2,5,3]-}
inversaPlegado2::[a]->[a]
inversaPlegado2 xs = foldl(\ acc x -> x:acc) [] xs

{-4.Definir una funcion para ordenar dos listas que recibe como paramentros-}
merge::Ord a=>[a]->[a]->[a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	|x<=y = x : merge(y:ys) xs
	|otherwise = y : merge(x:xs) ys
	
quickSort::Ord a =>[a]->[a]
quickSort [] = []
quickSort (x:xs) = 
	let pequenios = quickSort[z | z<-xs, z<=x]
	    mayores = quickSort [z | z<-xs,z>x]
	in pequenios ++ [x] ++ mayores
	
ordenaListas::Ord a =>[a]->[a]->[a]
ordenaListas (x:xs) (y:ys) =
	let primeraLista=quickSort(x:xs)
	    segundaLista=quickSort (y:ys)
	in merge primeraLista segundaLista
	
{-5.Se considera la funci�n
filtraAplica :: (a -> b) -> (a -> Bool) -> [a] -> [b]
tal que (filtraAplica f p xs) es la lista obtenida aplic�ndole a los elementos de xs que
cumplen el predicado p la funci�n f. Por ejemplo,
filtraAplica (4+) (<3) [1..7] => [5,6]-}
filtraAplica::(a->b)->(a->Bool)->[a]->[b]
filtraAplica f p = foldr (\x y -> if p x then (f x : y) else y)[]

{-6.Suma de las listas de una lista de listas-}
sumaDeListas::Num a =>[[a]]->a
sumaDeListas= foldr (\xs n ->sum xs +n)0

{-7.Lista obtenida borrando las ocurrencias de un elemento-}
borrarElemento::Eq a=>a->[a]->[a]
borrarElemento z = foldr(\x y ->if z==x then y else x:y)[]

{-8.Diferencia  de dos listas-}
diferencia::Eq a=>[a]->[a]->[a]
diferencia xs ys = foldl (flip borrarElemento) xs ys

{-8.Propiedades de elementos que verifican una propiedad-}
verificarPropiedad::Num a=>(a->Bool)->[a]->a
verificarPropiedad p = foldr (\x y -> if p x then x*y else y)1

{-9.Las cabezas y las colas de una lista-}
cabezas::[a]->[[a]]
cabezas = foldr (\x y ->[x]:[x:ys | ys <-y]) []

{-10.Pares ordenados por posicion
es la lista de todos los pares de elementos (x,y) de xs, tales que
x ocurren en xs antes que y. 
paresOrdenados [3,2,5,4] == [(3,2),(3,5),(3,4),(2,5),(2,4),(5,4)]
paresOrdenados [3,2,5,3] == [(3,2),(3,5),(3,3),(2,5),(2,3),(5,3)]-}
paresOrdenados::[a]->[(a,a)]
paresOrdenados [] = []
paresOrdenados (x:xs) = foldr (\y ac ->(x,y):ac)(paresOrdenados xs) xs

{-11.tal que (todos' p xs) se verifica si todos los elementos de xs cumplen la propiedad p. Por
ejemplo,
todos' even [2,6,4] ==> True
todos' even [2,5,4] ==> False-}
todos::(a->Bool)->[a]->Bool
todos p = foldr f True where f x y = p x && y


{-Con esto eliminamos elementos con foldr
eliminaP :: Int -> [Int] -> [Int]
eliminaP n = foldr f []
where f x y | rem x n == 0 = y
| otherwise = x:y-}


{-count e = foldr (\x acc -> if e ==x then acc+1 else acc)0-}
{-isAll e = foldr (\x acc ->e==x && acc) equivale a true-}
{-length=foldr(\x ->(+)1)0-}
{-map f = foldr ((:).f)[]-}