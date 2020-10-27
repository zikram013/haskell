
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

