
module Tema5DefinicionesDeTipos where

--Los type renombran
--Tambien permite definir nuevos tipos de datos mediante data
--Un tipo enumerado consta de un numero finito de valores, los cuales
--se enumeran en la definicion del tipo
--si ponenemos tipo y data hace falta poner el constructor. Ejemplo diapo 13 tipos producto

data Temperatura=Frio|Caliente deriving Show
data Estacion=Primavera|Verano|Otonyo|Invierno deriving Show

tiempo::Estacion->Temperatura
tiempo Primavera=Caliente
tiempo Verano=Caliente
tiempo _=Frio

data LetraOEntero=Letra Char | Entero Integer deriving Show
valorCaracter :: LetraOEntero
valorCaracter = Letra 'x'

valorEntero::LetraOEntero
valorEntero=Entero 23

--Arboles
data Arbol a = AV | Rama (Arbol a) a (Arbol a)
a1 = Rama (Rama (Rama AV 12 AV) 49--nodo raiz del 49 con nodo izq 12
		(Rama (Rama AV 23 AV) 5 (Rama AV 13 AV)))--nodo raiz 5 (der 49) con sus nodos 23,13
	123 (Rama AV 10 AV)--nodo raiz del todo con nodo 10 a la derecha
--calcular la lista de nodos choja de un arbol binario
hojas :: Arbol a -> [a]
hojas (Rama AV h AV) = [h]
hojas (Rama izq r der) = hojas izq ++ hojas der

--Tipos polimorficos
--tipo maybe
--data Maybe a =Nothing | Just a deriving...
division ::Int->Int->Maybe Int
division _ 0=Nothing
division m n =Just (m`div`n)

