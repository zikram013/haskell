
module PrimerosPasos where

--para divisiones se puede poner en la consola  mod y los numeros
--para sumas restas (-) (+)

doble :: Int -> Int
doble x = 2 * x

celsius :: Int -> Int
celsius f = (f-32)*5 `div`9

suma :: Int -> Int -> Int
suma x y = x + y
--continuacion de la suma
sucesor :: Int-> Int
sucesor = suma 1
sucesor2 :: Int->Int
sucesor2=(1 +)
--Funcion con suma y doble a la vez
composicion :: Int -> Int-> Int
composicion x y=(((suma x).(doble))y)
--suma2 :: (Int,Int)->Int
--suma2 (x,y) = x+y

multiploDe :: Integer -> Integer -> Bool
multiploDe p n= n `mod`p == 0
esPar :: Integer -> Bool
esPar=multiploDe 2

--operacion con tuplas
divEntera:: (Int,Int)->(Int,Int)
divEntera (m,n)=(m`div`n,m`rem`n)
--condicional
max::(Int,Int)->Int
max (m,n)= if m >n then m else n

--guardas
mayor::(Int,Int)->Int
mayor (x,y)
	|x>y =x
	|otherwise = y --otherwise siempre true