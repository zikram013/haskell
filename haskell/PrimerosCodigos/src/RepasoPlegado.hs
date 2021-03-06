
module RepasoPlegado where
--Factorial
factorial::Int->Int
factorial n = foldr (*) 1 [1..n]

--Suma de los elementos de una lista recursivo
sumaRecursiva::[Int]->Int
sumaRecursiva []=0
sumaRecursiva (x:xs)= x + sumaRecursiva xs

--Suma de los elementos de una lista mediante funciones de plegado
sumaPlegado:: [Int]->Int
sumaPlegado= foldr (+) 0

--Conjuncion sobre una lista de forma recursiva (En la disyuncion cambiar && pot || )
cunjuncionRecursiva::[Bool]->[Bool]
cunjuncionRecursiva []=True
cunjuncionRecursiva (x:xs)= x && cunjuncionRecursiva xs

--Conjuncion sobre una lista con funciones de plegado (En la disyuncion cambiar && pot || )
conjuncionPlegado::[Bool]->Bool
conjuncionPlegado = foldr (&&)True

--Redefinir la funcion foldr tal que foldr op  e l pliega por la derecha la lista l colocanado el operador op ente sus elementos y el elemento e al final
redefinicionFoldr::(a->b->b)->b->[a]->b
redefinicionFoldr f e [] = e
redefinicionFoldr f e (x:xs) = f x (redefinicionFoldr f e xs)

--Redefinir la funcion foldr tal que foldl op  e l pliega por la izquierda la lista l colocanado el operador op ente sus elementos y el elemento e al final
redefinicionFoldl:: (a->b->a)a->[b]->a
redefinicionFoldl f z [] =z
redefinicionFoldl f z (x:xs)=redefinicionFoldl(f z x) xs

--Concatenar Listas recursivo
concatenarRecursivo::[[a]]->[a]
concatenarRecursivo [] = []
concatenarRecursivo (xs:xss)=xs ++ concatenarRecursivo xss

--Concantenar listas con plegado
concatenarPlegado::[[a]]->[a]
concatenarPlegado= foldr (++) []

--concatenar listas Compresion
concatenarCompresion::[[a]]->[a]
concatenarCompresion xss =[x | xs <- xss, x<-xs]

--Obtener el ultimo elemento de una lista recursivo
ultimoElementoRecursivo::[a]->a
ultimoElementoRecursivo [x]=x
ultimoElementoRecursivo (_:xs)=ultimoElementoRecursivo xs

--Obtener el ulitmo elemento cde una lista con funciones de plegado
ultimoElementoPlegado::[a]->a
ultimoElementoPlegado = foldr (\x y ->y)

--Obtener ultimno elemento de una lista con head y reverse
ultimoElementoListaHeadReverse::[a]->a
ultimoElementoListaHeadReverse xs = head(reverse xs)

--Invertir una lista recursivo
invertirRecursivo::[a]->[a]
invertirRecursivo []=[]
invertirRecursivo (x:xs) = invertirRecursivo xs ++ [xs]

--Invertir lista recursivo con acumulador
invertirAcumulador::[a]->[a]
invertirAcumulador xs = invertirAcumuladorAux xs [] 
	where invertirAcumuladorAux [] ys =ys
	      invertirAcumuladorAux (x:xs) ys = invertirAcumuladorAux xs (x:ys)
		
--Invertir lista con funciones de plegado
invertirPlegado::[a]->[a]
invertirPlegado= foldl (flip(:)) []

--Longitud de una lista con recursion
longRecursivo::[a]->a
longRecursivo [] = 0
longRecursivo (_:xs)=1+longRecursivo xs

--Longigitud de una lista con funcion de pleagado
longPlegado::[a]->a
longPlegado = foldr (\x y -> y+1)0


--longitud de una lista con listas intensionales y sum
longIntensionales::[a]->a
longIntensionales xs= sum [1 | x<-xs]

--Elemento en una lista recurisvo (En caso de que no perteneceza cambiar los false por true y un distinto en los iguales)
perteneceRecursivo:: a->[a]->Bool
perteneceRecursivo [] = False
perteneceRecursivo x (y:ys) = (x==y)|| perteneceRecursivo x ys

--elemento en una lista con funcion de plegado (En caso de que no perteneceza cambiar los false por true y un distinto en los iguales)
pertenecePlegado::a->[a]->Bool
pertenecePlegado x = foldl (\z y ->z || x==y)False

--Insercion Ordenada de una elemenos
inserta:: a->[a]->[a]
inserta e [] = [e]
inserta e (x:xs) | e<=x = e:x:xs
				| otherwise = x :inserta e xs

--Ordenacion por insercion recurisvo
ordenacionInsercionRecurisvo :: [a]->[a]
ordenacionInsercionRecurisvo [] = []
ordenacionInsercionRecurisvo (x:xs)=inserta x (ordenacionInsercionRecurisvo xs)

--Ordenacion por insercion plegado
ordenacionInsercionPlegado:: [a]->[a]
ordenacionInsercionPlegado = foldr inserta []
--ordenacionInsercionPlegado = foldl (flip inserta)[]

--Minimo elemento de una lista recurisvo
minimoRecursivo :: [a]->a
minimoRecurisvo [x]=x
minimoRecursivo (x:y:xs)=minimoRecursivo ((min x y):xs)

--minimo elemento de una lista con un plegado
minimoPlegado::[a]->a
minimoPlegado=foldl min
