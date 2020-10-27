
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
