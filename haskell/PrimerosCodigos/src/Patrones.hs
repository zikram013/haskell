
module Patrones where

procesarURL :: String->String->String
procesarURL protocolo uri =
	if protocolo =="http" then "peticion por http: " ++ uri
	else if protocolo == "ftp" then "peticion por ftp: " ++uri
	else if protocolo =="sftp" then "peticion por sftp: " ++uri
	else "protocolo desconocido"
	
--Otra forma de definir
procesarURL2 :: String->String->String
procesarURL2 "http" uri="peticion por http: " ++uri
procesarURL2 "ftp" uri ="peticion por ftp: "++uri
procesarURL2 "sftp" uri="peticion por sftp: "++uri
procesarURL2 protocolo uri="protocolo desconocido"

--otro ejemplo de patrones para ver que pasa si el primero no se corresponde
f :: [Integer] ->Integer
f [1,x,y]=x+y
--si el primer numero no corresponde a 1 da fallo

--otro ejemplo
suma::[Integer] ->Integer
suma[]=0
suma[x]=x
suma [x,y]=x+y
suma[x,y,z]=x+y+z

--patron constante
patronConstante :: Integer -> Bool
patronConstante 1 = True
patronConstante 2 = False

--Patron de listas
patronLista :: [Integer] -> Integer
patronLista []=0
patronLista [x,y,z]=x+y+z

--PatronTuplas
primeroPatronTupla :: (Int,Int,Int) -> Int
--primeroPatronTupla t = t --esto valdria porque t representa en la tupla
primeroPatronTupla (x,y,z) = x
--primeroPatronTupla (x,y)=x esto no vale

--patron anidado
primeroPatronAnidado' ::[(Int,Int)] -> Int
primeroPatronAnidado' ((x,y):xs)=x

--patron Subrayado --no puede aparecer en la llamada de una funcion
isZero :: Int -> Bool
isZero 0 = True 
isZero _ = False

--Patron aritmetico
patronArimetico :: Int -> Int 
patronArimetico 0=0
patronArimetico (n+1) = n

--Patron Nombrado
patronNombrado :: Int-> Int
patronNombrado 0=0
patronNombrado (n+2)=(n+2)+3

patronNombrado2 :: Int->Int
patronNombrado2 0=0
patronNombrado2 m@(n+2)=m+3



