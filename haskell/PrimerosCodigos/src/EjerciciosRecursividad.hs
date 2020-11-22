
module EjerciciosRecursividad where

import Data.Char


binario :: Int->Int
binario numero = if numero ==0 then 0
		else if numero==1 then 1
		else (numero`mod`2) + 10*(binario(numero`div`2)) 

--binario a decimal
inversaBinarioAux :: Int->Int->Int
inversaBinarioAux 0_=0
inversaBinarioAux n pot= (n`mod`10)*(2^pot)+(inversaBinarioAux(n`div`10)(pot+1))

--numero primo
primo :: Int->Bool
primo n
	|n<=3=True
	|n`mod`2== 0 = False
	|otherwise=comprobarPrimos(n,3)


comprobarPrimos ::(Int,Int)->Bool
comprobarPrimos (n,i)
	|i*i>n=True
	|n`mod`i==0=False
	|otherwise=comprobarPrimos(n,i+2)

--numeroPerfecto

primos:: Int->Int->Bool
primos x n = ((mod x n) /= 0) && (primos x (n - 1))

perfectos::[Int]
perfectos = [x | x <- [2..], primos (2^x-1) (2^x - 2) , x <- [(2^(x-1))*(2^x-1)]]

--Dos numero son amigos si el numero de los diviores del primero dan al segundo y viciversa

--Ordenar una lista con insertar
insertar::Int->[Int]->[Int]
insertar elemento[] = [elemento]
insertar elemento(x:xs)
	| elemento<=x = elemento:x:xs
	| otherwise = x: insertar elemento xs
	
--ordenar lista segun alfonso
insercion::[Int]->[Int]
insercion[]=[]
insercion(x:xs)= insertarAlfonso x(insercion xs)

insertarAlfonso::Int->[Int]->[Int]
insertarAlfonso x[]=[x]
insertarAlfonso x(y:ys)= if(x<y)then (x:y:ys)else (y:(insertarAlfonso x ys))

--Ordenar con MergeSort
mergeSort::[Int]->[Int]
mergeSort[]=[]
mergeSort[x]=[x]
mergeSort l = fusion(mergeSort a,mergeSort b)
	where
		(a,b)= partir(l,[],[])
		
partir::([Int],[Int],[Int])->([Int],[Int])
partir([],l1,l2)=(l1,l2)
partir([x],l1,l2)=(x:l1,l2)
partir(a:b:xs,l1,l2)=partir(xs,a:l1,b:l2)

fusion::([Int],[Int])->[Int]
fusion(l,[])=l
fusion([],l)=l
fusion(a:as,b:bs) = if a<b then a:fusion(as,b:bs)else b:fusion(a:as,bs)

--secuencias de ceros
ceros::[Int]->Int
ceros []=0
ceros [0]=1
ceros [x]=0
ceros (0:0:xs)=ceros(0:xs)
ceros (0:x:xs)=1 + ceros(xs)
ceros (y:x:xs)= ceros(x:xs)

--Sumar los dobles de los elementos de una lista
sumarDoble ::(Num a) => [a]->a--Con el num a decimos que solo numeros
sumarDoble l = foldr (\ x y ->2*x + y) 0 l

sumarDoble2 ::(Num a) => [a]->a
sumarDoble2 l = foldl (\ x y -> 2*y + x) 0 l --Se cambian los papeles

--Dado un elemento y una lista añade el elemento al final de la misma
addAlFinal ::[a]->a->[a]
addAlFinal l n = foldr(\x y-> x:y)[n] l

--Elimina un cierto numero de la lista
eliminarElemento::[Int]->Int->[Int]
eliminarElemento lista candidato =foldl(\acumulador elemento -> if (elemento==candidato) then acumulador else acumulador ++[elemento]) [] lista 

--Examen
--Pertenece un elemento a una lista
pertenece ::[Int]->Int->Bool
pertenece lista candidato=foldl (\acum elemento -> (elemento==candidato)||acum) False lista -- ||provoca que cambie de false a true

--Apariciones de un elemento en una lista
aparece ::[Int]->Int->Int
aparece lista elemento= foldr(\x y -> if (x==elemento)then y+1 else y)0 lista

--cribar:elimina los multiplos de un numero
cribar::[Int]->Int->[Int]
cribar lista elemento=foldl(\acum x -> if(x`mod`elemento==0)then acum else acum++[x])[] lista

--Dada una lista de enteros devovler dos lsitas, una con elementos sin repetir y otra con los elementos repetidos
repetidos ::[Int]->([Int],[Int])
repetidos lista=foldr(\numero (x,y)-> if(aparece2(lista,numero)==1)then(numero:x,y)else(x,numero:y)) ([],[]) lista--Primer Elemento son los numeros sin repetir y el segundo los repetidos

aparece2 ::([Int],Int)->Int
aparece2 (lista, elemento)= foldr(\x y -> if (x==elemento)then y+1 else y)0 lista
--segmentos: forma de crecer de una lista devolver su crecimiento
crecimiento:: [Int]->[[Int]]
crecimiento [] = [[]]
crecimiento (x:xs) = []: [x:ys | ys <- crecimiento xs]
--crecimiento lista= foldr(\ elemento l-> if (comprobarVacia(lista)>0)then l:elemento (crecimiento(lista)) else elemento) [] lista

eliminarElemento3 ::([Int],Int)->[Int]
eliminarElemento3 (lista ,candidato) =foldl(\acumulador elemento -> if (elemento==candidato) then acumulador else acumulador ++[elemento]) [] lista 


comprobarVacia:: [Int]->Int
comprobarVacia []=0
comprobarVacia lista=length(lista)

eliminarElemento2::[Int]->[Int]
eliminarElemento2 lista =drop 1 (lista) 


--recibe un String y devolver en dos String: uno con las mayusculas y otro con las minusculas
mayusMinus :: String->([Char],[Char])
mayusMinus palabra =foldr(\letras (x,y)-> if isUpper letras then (letras:x,y)else (x,letras:y)) ([],[]) palabra

--funcion que recibe dos listas y va cofiendo un elemento de la primera y dos de la segunda, creando una lsita final de ternas
ternas::[a]->[b]->[(a,b,b)]
ternas []_=[]
ternas _[]=[]
ternas _[x]=[]
ternas (x:xs)(y1:y2:ys)=(x,y1,y2):(ternas xs ys)

--Lsita separada por cada 0 que te encuentras
separaCeros::[Int]->[[Int]]
separaCeros []=[[]]
separaCeros (0:0:xs) = separaCeros xs
separaCeros (0:x:xs) = [x:ys| ys <- separaCeros xs]
separaCeros (x:xs)=[]:[x:ys| ys <- separaCeros xs]
--formaalfonso
secuenciaCeros::[Int]->[[Int]]
secuenciaCeros []=[]
secuenciaCeros l = secuenciaAux l [] []


secuenciaAux:: [Int]->[Int]->[[Int]]->[[Int]]
secuenciaAux []acum resultado=resultado++[acum]
secuenciaAux (x:xs) acum resultado= if(x==0)then secuenciaAux xs[](resultado++[acum])
									else secuenciaAux xs (acum++[x])resultado

--funcion que devuleve una tupla con el elemnto no repetido y su posicion
posicion:: [Int]->[(Int,Int)]
posicion lista = posicionAux lista 0 [][]

posicionAux:: [Int] -> Int ->[Int] ->[(Int,Int)]->[(Int,Int)]
posicionAux [ ] _ _ res = res
--posicionAux (l:ls) pos control res = if pertenece2 l control then posicionAux ls (pos +1) control res
--                                                            else posicionAux ls (pos +1) (l:control) ( res ++ [ (l,pos)])


pertenece2 ::[Int]->Int->Bool
pertenece2 lista candidato=foldl (\acum elemento -> (elemento==candidato)||acum) False lista -- ||provoca que cambie de false a true

--tartaglia
binomio :: Int->[Int]
binomio 1=[1]
binomio n=[1]++[x+y | (x,y)<-sumas (binomio (n-1))]++[1]

sumas :: [Int]->[(Int,Int)]
sumas (x:y:xs) = (x,y):sumas(y:xs)
sumas _ =[]

--contar cuantas veces aparece un elemento
apariciones:: [Int]->Int->Int
apariciones lista elemento = foldl ( \ sumador x -> if x==elemento then sumador +1 else sumador ) 0 lista
--subconjuntossu 
subconjunto:: [Int]->[[Int]]
subconjunto []=[[]]
subconjunto (x:xs)=subconjunto xs ++[x:ys | ys <- crecimiento xs]

subconjuntos :: [Int]->[[Int]]
subconjuntos []=[[]]
subconjuntos (x:xs)=[x:ys | ys <- subconjuntos xs]++ subconjuntos xs

subconjuntoAlfonso::[a]->[[a]]
subconjuntoAlfonso []=[[]]
subconjuntoAlfonso l=foldl(\acum e->acum ++map (e:)acum )[[]] l

{--Implementa una funcion en haskell que elimine de una lista de entero aquellos números multiplo de x 
--}
listaMultiplo::Int->[Int]->[Int]
listaMultiplo x []=[]
listaMultiplo numero lista = foldl(\acum elemento-> if ((elemento `rem` numero)/=0)then acum++[elemento] else acum )[] lista

{--Se pide una funcion en haskell que dada una lista de numeros enteros obtenga un numero entero con el resultado de calcular el doble de cada uno de los elementos de la lista original
y sumarlos todos. Se debe hacer con recursividad final,no final y lambda--}
sumaDoblesFinal::[Int]->Int
sumaDoblesFinal []=0
sumaDoblesFinal (x:xs)=2*x + sumaDoblesFinal xs

sumaDoblesNoFinal ::[Int]->Int
sumaDoblesNoFinal (x:xs)=if((length xs)==0)then 0 else 2*x + sumaDoblesNoFinal xs

{--Implementa una funcion que sume los cuadrados de los numeros pares contenidos en una lista de numeros enteros. Se piden dos versiones
a. version que haga uso de las funciones de orden superior de listas map y filter para definir la nueva funcion
b. una version que utilice la definicion de listas por comprensión--}
doblesPorCompresion :: [Int]-> [Int]
doblesPorCompresion []= []
doblesPorCompresion (x:xs)= if ((x`rem`2)==0)then x:doblesPorCompresion xs else doblesPorCompresion xs  

doblesPorCompresionMap::[Int]->[Int]
doblesPorCompresionMap (x:xs)= x*x : (doblesPorCompresion xs)	

{-- Dada una lista de enteros, implementar una funcion para devolver tuplas formadas por los elementos sin repetir de la lista, junto con su posicion--}
numeroYposicion::[Int]->[(Int,Int)]
numeroYposicion xs = zip xs [0..]

{--Dada una lista de numeros enteros implementar una funcion que devuelva una lista con los n elementos mayores de la lista original--}
numerosMayores :: Int->[Int]->[Int]
numerosMayores filtro [] = []
numerosMayores filtro lista = foldl(\acum elemento-> if(elemento >filtro)then acum++[elemento]else acum)[] lista

{--Implementa una funciona incluye en haskell que reciba dos listas de numeros enteros y nos diga si la primera de las listas está  contenida en otra si los elementos de la primera 
aparecen dentro de la segunda, en el mismo roden y de forma consecutiva--}
contiene::[Int]->[Int]->Bool
contiene [] _ = True
contiene _ [] = False
contiene (x:xs) (y:ys) = (x==y && xs==(take(length xs)ys))|| (contiene(x:xs)ys)

{--Implementa una funcion polimorfica en haskell que reciba 2 listas y vaya cogiendo un elemento de la primera lista y dos de la segunda creando una lista final de tiernas.
Si una se acaba se mostrara la lista hasta ese momento--}
mezclarEnTernas::[Int]->[Int]->[(Int,Int,Int)]
mezclarEnTernas [] _ = []
mezclarEnTernas _ [] = []
mezclarEnTernas _ [x]=[]
mezclarEnTernas (x:xs)(y1:y2:ys)=(x,y1,y2) :(mezclarEnTernas xs ys)

{--Mediante la programacion de orden superior se pide implementar una de las funciones predefinidas en la libreria estandar de haskell: funcion zipWith
Esta funcion recibe como parametro una funcion y dos listas y une ambas listas aplicando la funcion entre los correspondientes parámetros--}
zipwith::(a -> b -> c)->[a]->[b]->[c]
zipwith funcion l1 l2 =[funcion x y | (x,y)<-zip l1 l2]

{--Hacer la funcion reverse usando recursion no final, final y folder--}
--final
invertirFinal::[a]->[a]
invertirFinal []= []
invertirFinal (x:xs) = invertirFinal xs ++[x]
--no final
invertirNoFinal ::[a]->[a]->[a]
invertirNoFinal [] l =[]
invertirNoFinal (x:xs) l= invertirNoFinal xs (x:l)
--foldr
invertirfoldr:: [a]->[a]
invertirfoldr l=foldr(\x xs ->xs ++[x])[]l

{--Define una funcion polimorfica que sea capaz de invertir los elementos de una lista de listas--}
revertirListas:: [[a]]->[[a]]
revertirListas l=map(invertirfoldr)(invertirfoldr l)

{--Implementar la funcion predefinida de la libreria estandar flip. Esta funcion lo que hace es recibir una funcion y devolver otra funcion que es identrica a la funcion original
salvo que intercambia los dos primeros parametros--}
funcionFlip:: (a->b->c)->b->a->c
funcionFlip funcion x y = funcion y x

{--Implementar la funcion polimorfica predefinida de la libreria estandar map. Esta funcion lo que hace es recibir una funcion y una lista y devuelve la lista resultante de aplicar la funcion
a cada elemento de la lista original--}
funcionmap :: (a->b)->[a]->[b]
funcionmap funcion lista = [funcion x | x <- lista]
