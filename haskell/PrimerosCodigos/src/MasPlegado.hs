
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