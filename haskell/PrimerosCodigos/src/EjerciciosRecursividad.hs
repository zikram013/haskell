
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