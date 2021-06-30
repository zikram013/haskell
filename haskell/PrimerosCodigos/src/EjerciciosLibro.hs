
module EjerciciosLibro where
import Data.Char
import Data.List

{-Factorial de un numero-}
--Condicional
factorialCondicional::Int->Int
factorialCondicional n = if n == 0 then 1 else n * factorialCondicional (n-1)
--Guardas
factorialGuarda::Int->Int
factorialGuarda n
	|n == 0 = 1
	|otherwise = n * factorialGuarda (n-1)
--Mediante patrones
factorialPatrones::Int->Int
factorialPatrones 0 = 1
factorialPatrones (n+1)= n * factorialPatrones n
--Predefinida
factorialPredefinida::Int->Int
factorialPredefinida n = product[1..]
--Plegado
factorialPlegado::Int->Int
factorialPlegado n = foldr (*) 1 [1..n]

{-Numero de combinacion n m = n!/k!*(n-k)!-}
numeroCombinaciones::Int->Int->Int
numeroCombinaciones n k = (factorialPlegado n) `div` ((factorialPlegado k)*(factorialPlegado(n-k)))

{-Suma de cuadrados-}
--con sum y listas intensionakl
sumaCuadradosIntensional::[Int]->Int
sumaCuadradosIntensional l = sum[x*x|x<-l]
--con sum y lambda
sumaCuadradoLambda::[Int]->Int
sumaCuadradoLambda l = sum (map(\x->x*x)l)
--SumaCuadradoConRecursion
sumaCuadradoRecursion::[Int]->Int
sumaCuadradoRecursion [] = 0
sumaCuadradoRecursion (x:xs)=x*x + sumaCuadradoRecursion xs

{-Raices de ecuaciones de segundo grado-}
--directa
raicesEcuacionDirecta::Double->Double->Double->[Double]
raicesEcuacionDirecta a b c = [(-b+sqrt(b*b-4*a*c))/(2*a),(-b-sqrt(b*b-4*a*c))/(2*a)]
--Entorno Local
raicesEcuacionLocal::Double->Double->Double->[Double]
raicesEcuacionLocal a b c =
	[(-b+d)/n,(-b-d)/n] 
		where d = sqrt(b*b-4*a*c)
		      n = 2*a

{-Redefinir la funcion signum tal que signum x es -1 si x es -1 si x es negativo, 0 si x es cero y 1 si x es positivo-}
n_signum::Int->Int
n_signum x | x>0 = 1
 			| x==0 = 0

{-Potencias-}
--patrones
potenciaPatrones::Num a =>a->Int->a
potenciaPatrones x 0 = 1
potenciaPatrones x (n+1) = x *(potenciaPatrones x n)
--condicionales
potenciaCondicionales::Num a =>a->Int->a
potenciaCondicionales x n = if n==0 then 1 else x * potenciaCondicionales x (n-1)

{-Redifinicion de la funcion map tal que map f l es la lista obtenida aplicando f a cada elemento l-}
--recursion
mapRecursivo::(a->b)->[a]->[b]
mapRecursivo f [] = []
mapRecursivo f (x:xs) = f x : mapRecursivo f xs
--lista intensional
mapIntensional::(a->b)->[a]->[b]
mapIntensional f xs = [f x | x<-xs]

{-Redefinir la funcion filter-}
--recursion
filterRecursivo::(a->Bool)->[a]->[a]
filterRecursivo p [] = []
filterRecursivo p (x:xs) | p x = x:filterRecursivo p xs
							|otherwise = filterRecursivo p xs
--Listas intensionales
filterIntensional::(a->Bool)->[a]->[a]
filterIntensional p xs = [x |x<-xs,p x]

{-Redifinir la funcion sum para sumar los elementos de una lista-}
--recursivo
sumRecursivo::Num a=>[a]->a
sumRecursivo []=0
sumRecursivo (x:xs) = x + sumRecursivo xs
--plegado
sumPlegado::Num a =>[a]->a
sumPlegado = foldr (+) 0

{-Defnir la funcion derivada tal que derivada a f x es el valor de la derivada f en el punto x con aproximacion a-}
derivada::Float->(Float->Float)->Float->Float
derivada a f x = (f(x+a)-f(x))/a

{-Definir la funcion igual lista tal que igual lista xs ys se verifica que sean iguales-}
igualLista :: Eq a => [a] -> [a] -> Bool
igualLista [] [] = True
igualLista (x:xs) (y:ys) = x==y && igualLista xs ys
igualLista _ _ = False

{-Definir la funcion conc tal que conc l1 l2 es la concatenacion de l1 y l2-}
conc::[a]->[a]->[a]
conc [] ys = ys
conc (x:xs) ys = x :(conc xs ys)

{-concatenar una lista de listas-}
--recursivo
concatRecursivo::[[a]]->[a]
concatRecursivo [] = []
concatRecursivo (xs:xss) = xs ++ concatRecursivo xss
--Plegados
concatPlegado::[[a]]->[a]
concatPlegado = foldr (++) []
--Compresion
concatCompresion::[[a]]->[a]
concatCompresion xss  = [x|xs<-xss,x<-xs]

{-Redefinir la funcion head tal que head l es la cabeza de la lista-}
n_head::[a]->a
n_head (x:_)=x

{-Redefinir la funcion tail -}
n_tail::[a]->[a]
n_tail(_:xs)=xs

{-redefinir la funcion last-}
--recursivo
lastRecursivo::[a]->a
lastRecursivo [x] = x
lastRecursivo (_:xs) = lastRecursivo xs
--plegado
redefinicionFoldr::(a->b->b)->b->[a]->b
redefinicionFoldr f e [] = e
redefinicionFoldr f e (x:xs) = f x (redefinicionFoldr f e xs)
lastPlegado::[a]->a
lastPlegado = redefinicionFoldr(\x y -> y)
--con head y reverse
lastHeadReverse::[a]->a
lastHeadReverse xs = head(reverse xs)

{-Lista sin el ultimo elemento-}
--recursivo
sinUltimoElementoRecursivo::[a]->[a]
sinUltimoElementoRecursivo (x:xs) = x : sinUltimoElementoRecursivo xs

{-Definir la funcion take tal que take n l es la lista de los n primeros elementos de l-}
n_take::Int->[a]->[a]
n_take n _ | n<=0 = []
n_take _ [] = []
n_take n (x:xs) = x : n_take(n-1) xs

{-Redefinir la funcion takeWhile tal que takeWhile p l es la lista de los elementos iniciales que verifian p-}
n_takeWhile::(a->Bool)->[a]->[a]
n_takeWhile p [] = []
n_takeWhile p (x:xs)
		| p x = x:n_takeWhile p xs
		|otherwise = []
		
{-Redefinir la funcion drop tal que drop n l es la lista obtenida eliminado los primero n elementos de la lista l-}
n_drop::Int->[a]->[a]
n_drop n xs | n <=0 = xs
n_drop _ [] = []
n_drop n (_:xs) = n_drop (n-1) xs

{-Redefinir drop while-}
n_dropWhile::(a->Bool)->[a]->[a]
n_dropWhile p [] = []
n_dropWhile p l@(x:xs)
			| p x = n_dropWhile p xs
			|otherwise = l
			
{-Definir la funcion nth que nth l n es el elemento enesimo de la lista-}
nth::[a]->Int->a
nth (x:_) 0 = x
nth (_:xs) n = nth xs (n-1)

{-Redefinir la funcion reverse-}
--recursion
reverseRecursivo::[a]->[a]
reverseRecursivo [] = []
reverseRecursivo (x:xs) = reverseRecursivo xs ++ [x]
--Recurisvo con acumulador
reverseAcumulador::[a]->[a]
reverseAcumulador xs =
	reverseAcumuladorAux xs []
	where reverseAcumuladorAux [] ys = ys
	      reverseAcumuladorAux (x:xs) ys = reverseAcumuladorAux xs(x:ys)
--Reverse con plegado
reversePlegado::[a]->[a]
reversePlegado = foldl (flip(:))[]

{-Redefinir la funcion lenght-}
--length recursivo
lengthRecursivo::[a]->Int
lengthRecursivo [] = 0
lengthRecursivo (_:xs) = 1 + lengthRecursivo xs
--lengt plegado
lengthPlegado::[a]->Int
lengthPlegado = foldr (\x y -> y+1) 0 --Por la derecha --lengthPlegado = foldr (\x y -> x+1) 0 Por la izquierda
--listaIntensinal
lengthIntensional::[a]->Int
lengthIntensional xs = sum [1|x<-xs]

{-Elemento que pertenece a una lista-}
--Plegado
pertenecePlegado::Eq a => a->[a]->Bool
pertenecePlegado x = foldl (\z y ->z || x==y)False -- si fuera que no pertenece foldl (\z y -> z && x/=y) True
--Intensional
perteneceIntensional::Eq a=>a->[a]->Bool
perteneceIntensional x ys = or [x==y |y<-ys]-- si fuera que no pertenece and [x/=y | y <- ys]

{-Comprobacion de que una lsita esta ordenada-}
listaOrdenada:: Ord a=>[a]->Bool
listaOrdenada [] =True
listaOrdenada[_]=True
listaOrdenada (x:y:xs) = (x<=y) && listaOrdenada(y:xs)

{-Igualdad de subconjuntos-}
igualdadConjuntos::Eq a =>[a]->[a]->Bool
igualdadConjuntos xs ys = aux (nub xs) (nub xs)
	where aux[] [] = True
	      aux(x:_) [] = False
	      aux [] (y:_)=False
	      aux (x:xs) ys = x `elem` ys && aux xs (delete x ys)
	      
{-Mezcla de dos listas ordenadas-}
mezclar :: Ord a =>[a]->[a]->[a]
mezclar [] ys = ys
mezclar xs [] = xs
mezclar (x:xs) (y:ys)
	|x<=y = x : mezclar xs (y:ys)
	|otherwise = y mezclar (x:xs)ys
	
{-Ordenacion por mezcla-}
ordenarMezcla :: Ord a=>[a]->[a]
ordenarMezcla [] = []
ordenarMezcla [x]=[x]
ordenarMezcla xs = mezclar (ordenarMezcla ys) (ordenarMezcla zs)
	where medio = (length xs) `div`2
	      ys = take medio xs
	      zs = drop medio xs