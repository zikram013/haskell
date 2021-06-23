
module AllEjercicios where
import Data.Char
import Data.List

--------Tema 2-------

{- Implementar una funcion en haskell que dados tres numeros enteros determine si estan ordenador de menor a mayor-}
estanOrdenados:: Int->Int->Int->Bool
estanOrdenados x y z = (x<y) && (y<z) && (x<z)

{-Implementar una funcion en Haskell que dados tres numeros enteros lo devuelva ordenados de menor a mayor-}
ordenarTresNumeros::Int->Int->Int->[Int]
ordenarTresNumeros x y z 
		|((x<y)&&(x<z)&&(y<z))=[x,y,z]
		|((x<y)&&(x<z)&&(y>z))=[x,z,y]
		|((y<x)&&(x<z)&&(y<z))=[y,x,z]
		|((y<x)&&(x>z)&&(y<z))=[y,z,x]
		|((x<y)&&(z<x)&&(z<y))=[z,x,y]
		|otherwise = [z,y,x]
		
{-Implementar en haskell una funcion que reciba un numero real y devuelva una tupla con su parte entera y sus dos primeros decimales(como numero entero)-}
tuplarDecimal::Float->(Int,Int)
tuplarDecimal n = (truncate n , mod(truncate(n*100))100)

{-Crear una funcion que reciba el readio de una circunferencia y devuelva una 2-tupla con la longitud de la circunferencia y con el area del circulo
Emplea una deficinion local con la clausula where para almacenar el valor de PI
Emplea despues una deficion local let para el mismo cometido-}
areaCirculo::Float->(Float,Float)
areaCirculo r = (2*pic*r , pic*r^2) where pic=3.14

areaCirculoLet::Float->(Float,Float)
areaCirculoLet r = let pic= 3.14 in (2*pic*r , pic*r^2)

{-Implementar la funcion predefinida de listas concatm que se llamara concatenar, utilizando la definicion de listas por compresion-}
concatenar::[[a]]->[a]
concatenar l = [y|x<-l , y<-x]

{-Implementar una funcion que dado un numero entero devuelva en una lista todos los factores de ese numero. Se debe utilizar la definicion de listas por compresion
Tambien llamado todos los divisores de un numero-}
factores::Int->[Int]
factores n = [x | x<-[1..div n 2], mod n x == 0]++ [n]

{-Implementar una funcion que diga si un numero es primo Para ello se deve utilizar la funcion que calcula el numero de factores-}
esPrimo::Int->Bool
esPrimo n = (length (factores n))==2

{-Implementar una funcion que diga cuantos caracteres en mayuscula estan contenidos en una frase dada. Se deberan utillizar listas por compresion-}
mayus::[Char]->Int
mayus frase = length([c | c <-frase, isUpper c])

{-Implementar una funcion que dada una tupla de tres elementos, donde cada uno de ellos es a su vez una tupla de dos elementos de tipo string e int, retorne
el primer elemento de cada tupla interna. Se debera utilizar ajuste de patrones-}
primerElemento:: ((String,Int),(String,Int),(String,Int))->[String]
primerElemento ((x1,x2),(y1,y2),(z1,z2))=[x1,y1,z1]

{-Implementar una funcion que devuelve True si la suma de los cuatro primeros elementos de una lista de numero entero ses un valor menor a 10 y devolvera False 
en caso contrario. Se debera utilizar ajuste de patrones-}
sumaMayor10::[Int]->Bool
sumaMayor10 l = (sum(take 4 l))<10

{-Implementar una funcion que dada una frase retorne un mensaje donde se indique-}
procesarFrase :: [Char]->[Char]
procesarFrase frase = "La letra frase "++[frase!!0]++ "la ultima letra: "++[last frase]

{-Implementar una funcion que dada una cadena de caracteres y un caracter, indique el numero de aparciones del caracter en la adena. No se debe utilizar recurisvidad
si ajuste de patrones. Utilizar por ejemplo listas de compresion -}
contarAparicion:: String->Char->Int
contarAparicion [] e = 0
contarAparacicion frase c = length([x| x<-frase, x==c])

-----Tema 4 -----------------
{-Implementa una funcion en haskell que elimine de una lista de enteros aquellos multiplos de x-}
--Por listas con compresion
eliminarMultiploCompresion::[Int]->Int->[Int]
eliminarMultiploCompresion l e = [x | x<-l, mod e x/=0]
--Recursividad no final
eliminarMultiploNoFinal::[Int]->Int->[Int]
eliminarMultiploNoFinal [] n = []
eliminarMultiploNoFinal (x:xs) n 
		|mod x n == 0 = eliminarMultiploNoFinal xs x
		|otherwise = x:eliminarMultiploNoFinal xs x
--Recurision final
eliminarMultiploFinal::[Int]->Int->[Int]->[Int]
eliminarMultiploFinal [] n ac = ac
eliminarMultiploFinal (x:xs) n ac
		|(mod x n)==0 =eliminarMultiploFinal xs n ac
		|otherwise = eliminarMultiploFinal xs n (ac++[x])
		
{-Dada la siguiente definicion de funcion
doble:: Int-> Int
doble x = x + x
Cambiarla para utilizar expresiones lambda-}
doble::Int->Int
doble = (\x->x+x)

{-Se pide una funcion en haskell que dada una lista de numeros enteros obtenga un numero entero con el resultado de calcular el doble de cada uno de los elementos-}
--Recursividad no final
sumaDobleNoFinal::[Int]->Int
sumaDobleNoFinal []=0
sumaDobleNoFinal (x:xs)= x*2 + (sumaDobleNoFinal xs)
--Recurisividad final
sumaDobleFinal::[Int]->Int->Int
sumaDobleFinal [] ac = ac
sumaDobleFinal (x:xs) ac = sumaDobleFinal xs (x*2 + ac)
--Con funciones lambda o de orden superior (se puede hacer uso de map)
sumaDobleLambdaOrdenSuperior::[Int]->Int
sumaDobleLambdaOrdenSuperior l = foldr (+) 0 (map (*2) l)

{-Implementa una funcion que sume los cuadrados de los numeros pares contenidos en un a lista-}
--Con map y filter
sumaCuadradoOrdenSuperior::[Int]->Int
sumaCuadradoOrdenSuperior l = sum(map(^2)(filter even l))
--Por listas de compresion
sumaCuadradoListaCompresion::[Int]->Int
sumaCuadradoListaCompresion l = sum [x^2 | x<-l,even x]

{-Dara una lista de enteros, implementar una funcion para devolver tuplas formadas por los elementos (sin repetir) de la lista junto con la primera posicion en la que aparecen-}
primeraPos::[Int]->[(Int,Int)]
primeraPos l = filter (\(ee,ii)->all(\(eee,iii)->(eee/=ee)||(ii<=iii))tuples)tuples
	where tuples = [(e,idx)| (e,idx)<- enumerate l]
		where enumerate x = zip x [1..]
		
{-Dada una lista de numeros enteros implementar una funcion que devuelva una lista con los n elementos mayores de la lista original-}
nMayores::[Int]->Int->[Int]
nMayores l n = [x | x<-l,length(filter(>n)l)<n]

{-Implementa una funcion incluye en haskell que reciba dos listas de numeros enteros y nos diga si la primera de la listas esta contenida en la segunda
se dice que una lista esta contenida en otra si los elementos de la primera aparecen en la segunda en el mismo orden y de forma consecutiva-}
listaContenida::[Int]->[Int]->Bool
listaContenida [] _ = True
listaContenida _ [] =False
listaContenida (x:xs)(y:ys) = (x==y && xs ==(take(length xs)ys)) || (listaContenida(x:xs)ys)

{-Dada una lista de enteros, se pide implementar una funcion que ordene dicha lista de menor a mayor utilizando un algoritmo de insercion
Dicho algoritmo consiste en recorrer la lista L, insertando cada elemento L[i] en el lugar correcto entre los elementos ya ordenador-}
insertar::Int->[Int]->[Int]
insertar num [] = [num]
insertar num (x:xs) = if(num<x) then num:x:xs else x:(insertar num xs)
insertarAux::[Int]->[Int]
insertarAux l = foldr (\x l2->insertar x l2)[]l 

{-Implementa una funcion polimorfica en haskell que reciba 2 listas y vaya cogfiendo un elemento de la primera y dos de la segunda, creando una lista final de ternas
En caso de que una de las dos lista se acaba, mostrara la lista de ternas construidas hasta ese momento-}
concatenarLista [] _ = []
concatenarLista _ [] = []
concatenarLista (x:xs) (y:z:zs) = (x,y,z):concatenarLista xs zs

{-Se pide una funcion polimorfica en haskell que dad un elemento y una lista añada dicho elemento al final de la lista-}
insertarFinal::a->[a]->[a]
insertarFinal n lista=lista ++ [n]

{-Mediante la programaion de orden superior se pide iplementar una de las funciones predefinidas en la libreria de haskell: zipwith
Esta funcion recibe como parametro una funcion y dos listas y une ambas listas aplicando la funcion entre los correspondientes paramentros-}
zipWith_::(a->b->c)->[a]->[b]->[c]
zipWith_ f l1 l2 = [f x y | (x,y)<-zip l1 l2]

{-Implementa una funcion en haskell que invierta una lista-}
--No final
invertirNoFinal::[a]->[a]
invertirNoFinal [] = []
invertirNoFinal (x:xs) = invertirNoFinal xs ++[x]
--Final
invertirFinal::[a]->[a]->[a]
invertirFinal [] l = l
invertirFinal (x:xs) l = invertirFinal xs (x:l)
--Foldl
invertirOrdenSuperior::[a]->[a]
invertirOrdenSuperior l = foldr(\x xs -> xs ++ [x])[] l
--Definirla de forma polimorfica que sea capaz de invertir los elementos de una lista de listas
invertirListaListas::[[a]]->[[a]]
invertirListaListas l = map(invertirOrdenSuperior) (invertirOrdenSuperior l)

{-Implementar la funcion flip. Esta funcion lo que hace es recibir una funcion y devovler otra funcion que es identica a la funcio original salvo que intercambia los dos primeros parametros-}
flip_::(a->b->c)->b->a->c
flip_ f y x = f x y

{-Implementar la funcion map. Esta funcion lo que hace es recibir una funcion y una lista que devuelve la lista resultante de aplicar la funcion a cada elemento de la lista original-}
map_::(a->b)->[a]->[b]
map_ f l = [f x | x<-l]