
module HojaDeEjerciciosDeDefinicionesDeTipos where
import Data.Char
import Data.List
{--Se pide una funcion que dada una lista de racionales, donde cada racional se define como dos numeros enteros, y un numero racional,devuelva otra losta con todos los racionales equivalentes
al dado.
con type y data--}
--con tipos donde racional es num y dem
type Racional =(Int,Int)
eq1 :: [Racional]->Racional->[Racional]
eq1 l (n1,n2)=[(x,y)|(x,y)<-l,n1*y == n2*x]
--con data
type Numerador=Int
type Denominador=Int
data Fraccion=F(Numerador,Denominador)
eq2:: [Fraccion]->Fraccion->[Fraccion]
eq2 l (F(n1,n2))=[(F(x,y)) |(F(x,y))<-l,n1*y==n2*x]

instance Show Fraccion where
	show(F(n1,n2))="F("++Prelude.show(n1)++","++Prelude.show(n2)++")"
	
{--Funcion que dado un punto de coordenadas y una direccion mueva el punto hacia la direccion indicada.--}
type Coordenada=(Float,Float)
data PuntoCardinal=Norte|Sur|Este|Oeste deriving Show
mover ::Coordenada->PuntoCardinal->Coordenada
mover (x,y) Norte=(x,y+1)
mover (x,y) Sur=(x,y-1)
mover (x,y)Este=(x+1,y)
mover (x,y)Oeste=(x-1,y)

{--funcion que dados dos puntos de coordeandas indique cual esta mas al sur--}
alSur::(Coordenada,Coordenada)->Coordenada
alSur ((x1,y1),(x2,y2))=if (y1>y2)then (x1,y1)else(x2,y2)

{--Calcular la distancia entre dos puntos--}
distancia::Coordenada->Coordenada->Float
distancia (x1,y1)(x2,y2) = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))

{--Funcion que dado un punto y una lista de direcciones,retorne el camino que forman todos os puntos despues de cada movimiento sucesivo dsde el punto original--}
camino::Coordenada->[PuntoCardinal]->[Coordenada]
camino (x,y) l=foldl(\r dir -> if (r==[]) then r ++ [mover (x,y) dir] else r ++ [mover (last r) dir])[] l

{--Definir una funcion que dado undia de la semana indique si es laborable o no. Se debe crear un tipo de dato enumerado--}
data Dia = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo deriving Show
laborable :: Dia->Bool
laborable Sabado=False
laborable Domingo=False
laborable _ =True

{--La empresa RealTimeSolutions esta trabajando en un controlador para una centras domotica. El controlador recibe informacion de termoestatos situados en diferentes habitaciones
de la vivienda y basandose en esta informacion, actica o desactiva el aire acondicionado en cada una de las habitaciones. Los termostatos pueden enviar la informacion sobre la temperatura
en celsius o farenheit. Los aparatos reciben dos ordenes. Off on--}
--a. Definir un tipo de datos para representar las temperaturas en ambos tipos de unidades
data Temperatura= Grados Double | Farents Double deriving Show
data Estado = On|Off deriving Show
--b.Definir una funcion convertir para convertir ambos tiempos de grados al otro
convertir::Temperatura->Double
convertir (Grados temp)=(temp *(9/5)+32)
convertir (Farents temp)=((temp-32)*5/9)
--c. DEfinir un tipo de datos para representar las ordenes a los aparatos de a/a y definir un metodo que lo controle
controlar::Temperatura->Estado
controlar(Grados temp)=if(temp<28)then On else Off
controlar (Farents temp)=if (convertir(Farents temp)<28)then On else Off

{--Definir un tipo moneda para representar euros y dolares. Definir una funcion que convierta entre ambas monedas sabiendo que el factor de conversion de euros a dolares es 1.14--}
data Moneda=Euros|Dolares
conversion :: Moneda->Double->Double
conversion Euros cantidad= cantidad *1.14
conversion Dolares cantidad= cantidad /1.14

{--Dada el siguiente tipo de datos recursivos que representa expresiones aritmeticas--}
data Expr=Valor Integer
	|Expr :+: Expr
	|Expr :-: Expr
	|Expr :*: Expr deriving Show
--Se pide una funcion par acaluclar el valor de una expresion
calcular:: Expr->Integer
calcular (Valor n)=n
calcular (v1 :+: v2)=calcular(v1)+calcular(v2)
calcular (v1 :-: v2)=calcular(v1)-calcular(v2)
calcular (v1 :*: v2)=calcular(v1)*calcular(v2)
--Se pide una funcion para calcular el numero de constantes de una expresion
constantes::Expr->Integer
constantes (Valor n)=1
contantes (v1 :+: v2)=constantes(v1)+constantes(v2)
contantes (v1 :-: v2)=constantes(v1)+constantes(v2)
contantes (v1 :*: v2)=constantes(v1)+constantes(v2)

{--Se quiere ordenar los elementos de una lista (cuyos elementos con comparables)mediente el algoritmo quickshort--}
quickshort:: (Ord a)=>[a]->[a]
quickshort []=[]
quickshort (x:xs)=(quickshort [y | y<- xs,y<x])++x:(quickshort [y|y<-xs,y>x])

{--Se pide implementar una funcion que dado un numero y una lista de numeros del mismo tipo, divida a ese numero por cada uno de los elementos contenidso en la lista y devuelva una lista con el
resultado--}
division::(Fractional a)=>a->[a]->[a]
division numero lista=[x / numero | x<-lista]

{--Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una universidad que pertenecezan a alguna asociancion
se deben crear los tipos de datos de estudiante, titulacion ,lista de estudiantes y lista de estudiandes que pernece a asocianceciaes--}
data Titulacion=Grado2|Grado2Ade|GradoAde deriving Show
type Nombre=String
data Estudiante= E Nombre Titulacion deriving Show
data Asociacion=Cultura|Deporte|Representacion
type ListaEstudiantes=[Estudiante]
type ListaEstudiantesAsociacion=[(Estudiante,Asociacion)]
instance Eq Estudiante where
	(E n1 t1)==( E n2 t2) = n1==n2

matriculado:: Estudiante->[Estudiante]->Bool
matriculado _ [] = False
matriculado e (x:xs) = if(e == x)then True else matriculado e xs	

estudianteEnAsociacion:: ListaEstudiantesAsociacion->[Estudiante]
estudianteEnAsociacion lista= map(\(x,y)->x)lista

mostrar::(ListaEstudiantes,ListaEstudiantesAsociacion)->[Estudiante]
mostrar(estu,estuAso)=[e | e <- estu ,matriculado e(estudianteEnAsociacion estuAso)]

{--Se quiere poder representar una fecha de la siguiente forma: dd/mm/yyyy, para ello se debera crear un nuevo tipo de dato. Si se crea un nuevo tipo de datos cuyo constructor de datos
es Fecha, en el interprete al poner fechas concreatas nos devolveria la representacion de la fecha que hayamos definido--}
data Fecha = Fe Int Int Int
instance Show Fecha where
	show (Fe day mes anyo) = show day ++ "/"++show mes++"/"++show anyo
	
{--Teniendo en cuenta el tipo de datos Fecha definido anteriormente se pide una funcion que compare dos fechas--}
instance Eq Fecha where
	(Fe d1 m1 a1)==(Fe d2 m2 a2) = (d1==d2) && (m1==m2) && (a1==a2)
	