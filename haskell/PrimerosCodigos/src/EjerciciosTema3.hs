
module EjerciciosTema3 where
import Data.Char
--Primer ejercicio
--�Estan los tres elementos ordenados?

ordenarNumeros :: (Int,Int,Int)->Bool
ordenarNumeros (x,y,z)=(x>=y)&&(y<=z)

--Segundo Ejercicoi
--�?ordena tres elementos
ordenados :: (Int,Int,Int)->(Int,Int,Int)
ordenados (x,y,z)
	|ordenarNumeros(x,y,z)=(x,y,z)
	|x<=y = if z>=x then(z,x,y) else (x,y,z)
	|z>=x=(y,x,z)
	|otherwise= if z>=y then (y,z,x)else(z,y,x)
	
--Ejercicio 3
--Real devuelto en parte entera y dos decimales

quedarnosReal :: Float -> (Int,Int)
quedarnosReal r =(truncate(r),truncate(r*100)-100*truncate(r))

--ejercicio4
--dado un numero se devuelven sus factores en una lista. Definicion de 
--listas por comprension
factores :: Int ->[Int]
factores n =[x|x <- [1..n], n `mod` x == 0 ]
--numeros primos
primos:: Int->Bool
primos n=length(factores n)==2
--factores n== [1,n]

--ejercicio 5: mayusculas en una frase dada
mayusculas:: String -> Int
mayusculas s= length[letras|letras <- s,isUpper letras]

--ejercicio 6:
--funcion que reciba una tupla de tres elementos donde a su vez sea tuplas de tipos string int
tuplas::((String,Int),(String,Int),(String,Int))->(String,String,String)
tuplas((x1,x2),(y1,y2),(z1,z2))=((x1,y1,z1))

--Funcion que cuenta cuantas veces se repita un caracter
repetirCaracter :: (String,Char)->Int
repetirCaracter ([],_)=0
repetirCaracter (palabra,letra) = length[x | x <- palabra, x==letra]

--Crear una funcion que reciba el radio de una circunferencia y devuelva una 2-tupla con la longitud de la circunferencia y con el area del circula
circulo :: Float->(Float,Float)
--circulo radio = (2*pis*radio,radio*radio*pis) where pis=3.14
circulo radio=let pis=3.14 in (2*pis*radio,radio*radio*pis)

--Implementar una funcion que dado un caracter, represente su punto cardinal correspondiente
puntoCardinal :: Char->String
puntoCardinal punto =
	if punto == 'N' then "Norte"
	else if punto == 'E' then "Este"
	else if punto == 'O' then "Oeste"
	else if punto == 'S' then "Sur"
	else "Punto cardinal inexistente"
	
--Implementar una funcion que dado un numero entero devuelva mensajes indicando que rango de valores se encuentra dicho numero usando funciones locales.
rangoValores :: Int->String
rangoValores valor =
	if valor < 10 then frase++" es menor a 10" 
	else if valor >= 10 && valor<20 then frase++" es mayor igual a 10 y menos a 20" 
	else frase++" es mayor igual  a 20" 
	where frase="El valor de la entrada"
	
--implementar la funcion predefinidad de listas concat, utilziando la definicion de la slistas por compresion
concatenar :: [[Int]]->[Int]--con este ejercicios unimos todas las sublistas en una
concatenar lista=[x | sublista <- lista ,x <- sublista]

--Primera letra y ultima letra
frase :: String->String
frase [] ="Frase vacia"
frase f = [head f]