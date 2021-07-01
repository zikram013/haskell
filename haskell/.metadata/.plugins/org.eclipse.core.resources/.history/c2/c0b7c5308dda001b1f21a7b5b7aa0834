
module EntradaSalida where
import Data.Char

holaMundo::IO()
holaMundo=putStr "hola mundo"

holaMundo2::IO()
holaMundo2=print "hola mundo"

main::IO()
main=interact (map toUpper)

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

	