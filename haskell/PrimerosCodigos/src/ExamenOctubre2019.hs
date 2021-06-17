
module ExamenOctubre2019 where

--triangulo de tartaglia
binomio :: Int->[Int]
binomio 1=[1]
binomio n=[1]++[x+y | (x,y)<-sumas (binomio (n-1))]++[1]

sumas :: [Int]->[(Int,Int)]
sumas (x:y:xs) = (x,y):sumas(y:xs)
sumas _ =[]

--Recibir una lista de elementos que pueden estar repetidos y muestre una lista de duplas con los elementos de lista y la posicion que aparecen
--por primera vez en la lista
primeraPos :: [Int] -> [(Int, Int)]
primeraPos l = filter (\(ee, ii) -> all (\(eee, iii) -> (eee /=  ee) || (ii <= iii)) tuples) tuples 
		where tuples = [(e, idx) | (e,idx) <- enumerate l]
			where enumerate x = zip x [1..]
-- el mismo pero con foldl
esta_en_resultado :: (Eq a) => a -> [(a,Int)] -> Bool
esta_en_resultado _ [] = False
esta_en_resultado e ((x,idx):es) = if (e == x) then True else esta_en_resultado e es
primeraAparicion :: (Eq a) => [a] -> [(a,Int)]
primeraAparicion l = foldl (\resultado (e,idx) -> if (esta_en_resultado e resultado) then resultado else resultado ++ [(e,idx)]) [] (zip l [1..])