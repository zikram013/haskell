
module Tema4Recursividad where

import Data.Char

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