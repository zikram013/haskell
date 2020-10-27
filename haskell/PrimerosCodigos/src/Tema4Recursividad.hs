
module Tema4Recursividad where

import Data.Char

--x:xs->donde x es el elemento y xs la lista
--La diferencia entre foldr y foldl es el orden en que empiezan a trabajar
--foldr empieza por la derecha
--foldl empieza por la izquierda
--polimorfismo: cambiar la variable por un parametro cualquiera. Siempre en minuscula

--funcion recursiva
factorial ::Int->Int
factorial n= 
	if n == 0 then 1 
	else n*factorial(n-1)
	
factorial2:: Int->Int
factorial2 0=1
factorial2 n=n*factorial2(n-1)

fibonacci :: Int->Int
fibonacci 0=1
fibonacci 1=1
fibonacci n=fibonacci(n-1)+fibonacci(n-2)

sumaLenta ::(Int,Int)->Int
sumaLenta (x,y)= if x>0 then sumaLenta(x-1,y+1)
			else y
			
--funciones de orden superior
incremento :: Int -> Int
incremento x=x+1

dosVeces ::(Int -> Int)->Int->Int
dosVeces f x = f(f x)

--funciones de orden superior , funciones de plegado

sumaLista::[Int]->Int
sumaLista [] =0
sumaLista(x:xs)=x+sumaLista xs

productoLista::[Int]->Int
productoLista[]=0
productoLista [x]=x
productoLista(x:xs)=x*productoLista xs

--concatenacion con foldl
concatenar::[[Int]]->[Int]--el parametro que entra es una lista dentro de otra
concatenar=foldr(++)[]

--Expresiones Lmabda (\ mas los parametros->)
invertirLista :: [Int]->[Int]
invertirLista = foldr (\x lista ->lista ++[x])[]--Esto siempre coincide con el valor devuelto
