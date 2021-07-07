
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
insertar1::Int->[Int]->[Int]
insertar1 num [] = [num]
insertar1 num (x:xs) = if(num<x) then num:x:xs else x:(insertar1 num xs)
insertarAux::[Int]->[Int]
insertarAux l = foldr (\x l2->insertar1 x l2)[]l 

{-Implementa una funcion polimorfica en haskell que reciba 2 listas y vaya cogfiendo un elemento de la primera y dos de la segunda, creando una lista final de ternas
En caso de que una de las dos lista se acaba, mostrara la lista de ternas construidas hasta ese momento-}
concatenarLista [] _ = []
concatenarLista _ [] = []
concatenarLista (x:xs) (y:z:zs) = (x,y,z):concatenarLista xs zs

{-Se pide una funcion polimorfica en haskell que dad un elemento y una lista a�ada dicho elemento al final de la lista-}
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

-------------Tema 5 ---------------
{-Se pide una funcion que dada una lista de racionales, donde cada racional se define como dos numero enteros (num/den), y un numero racional, 
devuelva otra lista con todos los racionales equivalentes al lado-}
--Con type
type Racional1=(Int,Int)
listaRacionalType::[Racional1]->Racional1->[Racional1]
listaRacionalType l (num,dem) = [(n,d)|(n,d)<-l, num*d==dem*n]
--Con Data
type Numerador = Int
type Denominador = Int
data (Racional2) = R(Numerador,Denominador)
listaRacionalConData::[Racional2]->Racional2->[Racional2]
listaRacionalConData l (R(num,dem))= [(R(n,d))|(R(n,d))<-l,num*d==dem*n]
instance Show Racional2 where
	show(R(num,dem))="R("++Prelude.show(num)++","++Prelude.show(dem)++")"
	
{-Funcion que dado un punto de coordenadas y una direccion mueva el punto hacia la direccionindicada. Un ejemplo de apliacion seria-}
type Coordenada = (Float,Float)
data PuntoCardinal = Norte| Sur| Este| Oeste deriving Show
mover::Coordenada->PuntoCardinal->Coordenada
mover(x,y) Norte=(x,y+1)
mover(x,y) Sur=(x,y-1)
mover(x,y) Este=(x+1,y)
mover(x,y) Oeste=(x-1,y)
--Funcion que dados dos puntos de coordenadas indique cual esta mas al sur.
masSur::Coordenada->Coordenada->Coordenada
masSur(x1,y1)(x2,y2) = if(y1>y2)then(x2,y2)else(x1,y1)
--Funcion que calcula la distancia entre dos puntos
distancia::Coordenada->Coordenada->Float
distancia(x1,y1)(x2,y2) = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
--Funcion que dad un punto y una lista de direcciones, retorne el caminno que forman todos los puntos despues de cad movimiento sucesivo desde el punto original
camino::Coordenada->[PuntoCardinal]->[Coordenada]
camino(x,y)l=foldl(\r dir -> if(r==[])then r ++ [mover(x,y)dir] else r ++[mover(last r)dir])[] l

{-Definir una funcion que dad un dia de la semana, indique si este es o no laborable. Para representar el dia de la semana se debera crear un nuevo tipo enumerado-}
data Dia = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo
laborable Sabado=False
laborable Domingo=False
laborable _ = True

{-Conversion de temparatura celsius farenheti con data y controlar el estado-}
data Temperatura = Celsius Double | Faren Double deriving Show
data Estado = On|Off deriving Show

convertir :: Temperatura->Double
convertir (Celsius temp)=(temp * (9/5)+32)
convertir (Faren temp) = ((temp-32)*5/9)

controlar::Temperatura->Estado
controlar (Celsius temp) = if (temp<28)then On else Off
controlar (Faren temp) = controlar (Celsius(convertir ( Faren temp)))

{-Definir un tipo moneda para representar euros y dolares. Definir una funcion que conveirte entre ambas monedas sabiendo que el factor de conversion de euros
a dolares es 1.14-}
data Moneda = Euro|Dolar
conversor::Moneda->Double->Double
conversor Euro n = n*1.14
conversor Dolar n = n/1.14

{-Dada el siguiente tipo de datos recurisvo que representa expresiones aritmeticas-}
data Expr = Valor Integer
			|Expr :+: Expr
			|Expr :-: Expr
			|Expr :*: Expr
evaluar :: Expr->Integer
evaluar (Valor n) = n
evaluar (e1 :+: e2) = evaluar (e1) + evaluar(e2)
evaluar (e1 :-: e2) = evaluar (e1) - evaluar(e2)
evaluar (e1 :*: e2) = evaluar (e1) * evaluar(e2)

{--Se pide una funcion para calcular el numero de constatntes de una expresion--}
contar::Expr->Integer
contar (Valor n)=1
contar(e1 :+: e2) = contar(e1) + contar(e2)
contar(e1 :-: e2) = contar(e1) + contar(e2)
contar(e1 :*: e2) = contar(e1) + contar(e2)

{-Se quiere ordenar los elementos de una lista (cuyos elementos con comparables), mediante el algoritmo de quicksort-}
quicksort::(Ord a)=>[a]->[a]
quicksort [] = [] 
quicksort (x:xs) = (quicksort [y | y <- xs, y < x]) ++ x:(quicksort [ y| y <- xs, y>x])

{-Se pide implementar una funcion que dad un numero (de cualquier tipo que soporte la operacion division) y una lista de numeros del mismo tipo, divida a ese numero
por cada uno de los elementos contenidos en la lista y devuelva una lista con el resultado-}
divide::(Fractional a)=>a->[a]->[a]
divide n l = [x/n | x<-l]

{-Dado un nuevo tipo de dato para representar un arbol binario de cualquier tipo-}
data Arbol a = AV | Rama (Arbol a) a (Arbol a)
instance(Show a)=>Show (Arbol a)where
	show AV = "*"
	show (Rama i r d) = "("++show i ++ "-|"++show r ++ "|-"++show d ++")"
--Implementar funcion espejo del arbol
espejo::Arbol a -> Arbol a
espejo AV = AV
espejo (Rama i r d )= Rama (espejo d) r (espejo i)

{-Se quiere poder mostrar por pantalla los datos de los estudiantes matriculados en una universidad que pertenezcan a alguna de las asociaciones de esta
Para ello se deberan crear nuevos tipo s de datos que representen-}
data Titulacion = Grado_II | Grado_II_ADE | Grado_ADE deriving Show
type Nombre = String
data Estudiante = Est Nombre Titulacion deriving Show
data Asociacion = Cultural | Deportiva | Representacion deriving Show
type EstudianteMatriculado = [Estudiante]
type Est_Aso=[(Estudiante,Asociacion)]
instance Eq Estudiante where
	(Est n1 t1) == (Est n2 t2) = n1 == n2
	
enAsociacion::Estudiante->[Estudiante]->Bool
enAsociacion _ [] = False
enAsociacion e (x:xs)= if (e==x) then True else enAsociacion e xs 

estudianteEnAso::Est_Aso->[Estudiante]
estudianteEnAso a = map(\(x,y)->x)a

mostrar :: (EstudianteMatriculado,Est_Aso)->[Estudiante]
mostrar (m,a) = [e|e<-m,enAsociacion e (estudianteEnAso a)]

{-Se quuiere poder representar una fecha de la siguiente forma: dd/mm/aaaa, para ello se dbera crear un nuevo tipo de dato.-}
data Fecha = F Int Int Int
instance Show Fecha where
	show (F dia mes anyo)=show dia ++ "/"++show mes ++"/"++show anyo
--Teniendo en cuenta esta definicion de funcion implementa la instancia EQ
instance Eq Fecha 
	where (F d1 m1 a1) == (F d2 m2 a2) = (d1==d2) && (m1==m2)&& (a1==a2)
--Ordenar listdo de fecha con quicksor
instance Ord Fecha where
	compare(F d1 m1 a1) (F d2 m2 a2) = if ((compare a1 a2)==EQ) then(if ((compare m1 m2)==EQ) then compare d1 d2 else compare m1 m2) else compare a1 a2
	
--Se pide crear una nueva clase de tipos llamada coleccion para representar colecciones de datos
--de datos de cualquier tipo. 
--Comportamiento->esVacia,insertar,primero,eliminar,size
class Coleccion c where
	esVacia:: c a -> Bool
	insertar::c a->a-> c a
	primero:: c a -> a
	eliminar:: c a -> c a
	size:: c a-> Int

data Pila a = Pil [a] deriving Show
data Cola a = Col [a] deriving Show

instance Coleccion Pila where
	esVacia (Pil p)= length p ==0
	insertar (Pil p) e=Pil(p++[e])
	primero (Pil p) = last p
	eliminar (Pil p)= Pil(init p)
	size (Pil p)= length p
	
--instance Coleccion Cola where
--	esVacia(Col c)=length c==0
--	insertar(Col c) e=Pil(c++[e])
--	primero(Col c)=head c
--	eliminar(Col (c:cs))= Col cs
--	size (Col c)=length c

{-Binomio de tartaglia o triangulo numerico-}
calcular::[Int]->[Int]
calcular [] = []
calcular [_] = []
calcular (x:y:zs)= (x+y):(calcular(y:zs))

triangulo::Int->[Int]
triangulo 0 = [1]
triangulo 1 = [1,1]
triangulo 2= [1,2,1]
triangulo n = 1:(calcular(triangulo (n-1))) ++[1]

{-Primera aparicion-}
esta::(Eq a)=>a->[(a,Int)]->Bool
esta _ [] = False
esta e ((x,idx):es)=if (e==x) then True else esta e es
primeraAparicion::(Eq a)=>[a]->[(a,Int)]
primeraAparicion l = foldl(\resultado(e,idx)-> if(esta e resultado)then resultado else resultado++[(e,idx)])[](zip l [1..])

{-Ejercicio de los poliniomis-}
data Monomio = M(Int,Int)
data Polinomio = P[Monomio]
--Instanciar la clase show
instance Show Monomio where
	show(M(a,b))=(show a)++"x^"++(show b)

instance Show Polinomio where
	show (P (m:ms)) = show m ++ (if ms == [] then "" else "+" ++ show (P ms))

--Instanciar la clase eq
instance Eq Monomio where
	(M(a1,b1))==(M(a2,b2)) = (a1==a2 && b1==b2)
	
instance Eq Polinomio where
	(P [])==(P[])=True
	(P [])==(P _)=False
	(P _)== (P [])= False
	(P(m1:ms1))==(P(m2:ms2))= (m1==m2)&&(ms1==ms2)

--Instanciar clase ord
instance Ord Monomio where
	compare(M(a1,b1))(M(a2,b2))= if cmp==EQ then compare a1 a2 else cmp where cmp = compare b1 b2

instance Ord Polinomio where
	compare(P []) (P[])=EQ
	compare(P []) (P _)=LT
	compare (P _) (P [])=GT
	compare(P(m1:ms1)) (P(m2:ms2))= if cmp == EQ then compare ms1 ms2 else cmp where cmp= compare m1 m2
	
{-Implementar la funcion evaluaPolinomio que recibvira un valor entero y un polinomio y nos devolvera su resultado-}
evaluaPolinomio::Int->Polinomio->Int
evaluaPolinomio x (P [])=0
evaluaPolinomio x(P(M(a,b):ps))=(a*(x^b)) + (evaluaPolinomio x(P ps))

{-Implementar la funcion inserta monomio que recibira un monomio y un polinomio y lo insertara en el mismo-}
concac::Monomio->Polinomio->Polinomio
concac m (P l) = P(m:l)
insertarMonomio::Monomio->Polinomio->Polinomio
intertarMonomio m (P [])= P[m]
insertarMonomio m(P(x:xs))=if m>x then (P(m:x:xs))else concac x (intertarMonomio m(P xs))

{-Funcion eliminar Monomio-}
eliminaMonomio::Int->Polinomio->Polinomio
eliminaMonomio n (P[]) = (P [])
eliminaMonomio n (P((M(a,b)):ms))
	|n == b = P ms
	|n>b=P((M(a,b)):ms)
	|otherwise = concac (M(a,b)) (eliminaMonomio n(P ms))
	
--Funcion que pida un nombre y lo devuelve en mayusculas y en minusculas
fnombre::IO()
fnombre=do
	putStrLn"Dame tu nombre"
	nombre<-getLine
	let mayus=map toUpper nombre
	let minus=map toLower nombre
	putStrLn("mayus-> "++mayus)
	putStrLn("minus-> "++minus)

--funcion que pida dos numeros enteros y si quiere sumarlos o restarlos
calculadora:: IO( )--IO(Int)

calculadora = do

               putStrLn("Dame el primer operando");

               ope1<-getLine

               putStrLn("Dame el segundo operador");

               ope2<-getLine

               putStrLn("Quieres sumar ('s') o restar('r')?")

               ope<-getChar

               let ope1a= ((read ope1)::Int)

               let ope2a= ((read ope2)::Int)

               case ope of

                  's'->putStrLn(show(ope1a+ope2a))--return (ope1a+op2a)

                  'r'->putStrLn(show(ope1a-ope2a))

                  _ ->do
                       putStrLn("No tengo esa operacion")
                       calculadora  
                       
                       
                       
esVocal::Char->Bool

esVocal a = (a=='a')||(a=='e')||(a=='i')||(a=='o')||(a=='u')



separar::String->(String,String)

separar s = foldr(\letra (v,c) -> if esVocal letra then (letra:v,c) else (v,letra:c))([],[])s
 
archivos::IO()
archivos=do
 	putStrLn("Fichero de entrada")
 	fentrada<-getLine
 	putStrLn("fichero de salido")
 	fsalida<-getLine
 	cad<-readFile fentrada
 	let (vocal,otros)=separar cad
 	writeFile fsalida vocal
 	appendFile fsalida otros
				
--Funcion que te vaya pidiendo lineas por pantalla y vaya mostrando la longitud hasta
--que la linea se vacia
contarLineas::IO()
contarLineas=do
	putStrLn"Dime la linea"
	frase<- getLine
	let longitud = length frase
	if(longitud ==0) then putStrLn "Fin del programa"
		else do
				putStrLn ("La frase tiene: "++ show longitud)
				contarLineas
--pide frases y las vaya dando la vuleta hasta que se le introduzca una frase nula
--usar words para separar na frase en palabras y unwords para la hacer la inversa

invertir::IO()
invertir=do
	putStrLn"Dime la linea"
	frase <- getLine
	let f= unwords(map reverse(words(frase)))--Dividimos frase, lo mapeamos y damos la vuelta, lo volvemos a juntas
	putStrLn f
	if(length f==0) then putStrLn"adios"
		else do
			putStrLn f
			invertir
			
--Llama a la funcion con un numero por pantalla y que lo intente adivinar
adivinar::Int->IO()
adivinar n= do
	putStrLn"Dime el numero"
	num <- getLine
	let numero = ((read num)::Int)
	if(numero==n)then 
		putStrLn "acertaste"
			else if (numero<n)then do
				putStrLn "es menor"
				adivinar n
			else do
				putStrLn"mayor"
				adivinar n
				
--Adivinar con binding
adivinar2::Int->IO()
adivinar2 n=putStrLn("Dime numero")>>getLine>>=(\numero->if((read numero)==n)then putStrLn("encontrado")else(putStrLn("otro")>>(adivinar2 n)))

--dos archivos, cada numero de cada archivo y sumarlo
sumasDesdeFichero::IO()
sumasDesdeFichero=do
	putStrLn("Fichero numero 1")
	fichero1<-getLine
	putStrLn("fichero numero 2")
	fichero2<-getLine
	texto1<-readFile fichero1
	texto2<-readFile fichero2
	let numero1=map read(words texto1)
	let numero2=map read(words texto2)
	--let minimo=if(length numero1)<length numero2)then (length numero1)else(length numero2)
	auxiliar numero1 numero2  1
	
auxiliar::[Int]->[Int]->Int->IO()
auxiliar [] _ _=putStrLn("adios")
auxiliar _[]_=putStrLn("adios")
auxiliar(x:xs)(y:ys)contador = do
								putStrLn("operacion"++show contador++":"++show(x+y))
								auxiliar xs ys (contador+1)
								
								
{-Ejercicios Random de internet-}
{-Definir una funcion que calcule el promoedio de 3 numeros recibidos como argumentos:-}
promedioTres::Int->Int->Int->Float
promedioTres n m p = fromIntegral(n+m+p)/3.0

{-4) Definir las dos siguientes funciones:
a) una funci�n min que calcule el m�nimo de 2 n�meros enteros. 
b) una funci�n minTres que calcule el m�nimo de 3 n�meros enteros usando la funci�n min del apartado a) aplic�ndola con notaci�n prefija.
c) idem pero con notaci�n infija.-}
mini::Int->Int->Int
mini x y 
	|(x<=y)=x
	|otherwise = y

minTres::Int->Int->Int->Int
minTres x y z = mini (mini x y)z

minTres2::Int->Int->Int->Int
minTres2 x y z = (x`mini`y) `mini`z

{-5.Definir una funci�n que devuelva el n�mero central de tres n�meros 
(mediana),usando una funci�n estanOrd::Int -> Int -> Int -> Bool 
que devuelve true si el segundo argumento es mayor o igual que el primero y menor o igual que el tercero-}
estanOrd:: Int-> Int ->Int -> Bool
estanOrd x y z = (x <= y) && (y <= z)

mediana2 :: Int-> Int ->Int -> Int
mediana2 x y z
	| estanOrd x y z || estanOrd z y x = y
	| estanOrd y z x || estanOrd x z y = z
	| estanOrd y x z || estanOrd z x y = x
	
{-6.Define la funci�n howManyEqual, que recibe 3 argumentos y devuelve cu�ntos de ellos son iguales.
howManyEqual 34 25 36 = 0
howManyEqual 34 25 34 = 2
howManyEqual 3 3 3 = 3-}
howManyEqual::Int->Int->Int->Int
howManyEqual n m p 
	|(n==m) && (m==p)=3
	|(n==m) || (m==p) || (n==p) = 2
	|otherwise = 0

{-7.Se pide una funci�n que dado un n�mero entero devuelva verdadero si est� entre 0 y 9 (ambos excluidos) y falso en caso contrario.
 Se piden tres versiones, una de ellas utilizando la expresi�n condicional if, 
y otra donde se utilicen guardas-}
numEntre1y8a :: Int -> Bool
numEntre1y8a n = if (n > 0) && (n < 9) then True else False

numEntreGuarda::Int->Bool
numEntreGuarda n
	|(n>0) &&(n<9)=True
	|otherwise = False
	
{-8.Definir una funci�n charToNum :: Char -> Int, que convierte un car�cter d�gito
 como �8� a su valor 8. Si no es un d�gito devuelve -1.-}
num:: Char -> Int
num  n 
	| isDigit n  = ord n - ord '0'
	| otherwise = -1
	

