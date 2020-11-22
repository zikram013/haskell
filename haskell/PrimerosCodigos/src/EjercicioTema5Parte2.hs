
module EjercicioTema5Parte2 where

--Instancias el data Arbol a la clase Eq
--data Arbol1 a=Nodo a (Arbol a)(Arbol a) | Vacio deriving (Show)	
data Arbol a=AV|Nodo a(Arbol a)(Arbol a) deriving (Show)
instance (Eq a)=>Eq(Arbol a)where
	AV==AV=True
	AV==_=False
	_==AV=False
	(Nodo r1 izq1 der1)==(Nodo r2 izq2 der2) = (r1==r2)&&(izq1==izq2)&&(der1==der2)

--Define un tipo natural e instacialo a las clases Eq y Ord
data Natural=Cero|Suc Natural deriving Show
instance Eq Natural where
	Cero==Cero=True
	Cero==_=False
	_==Cero=False
	(Suc n)==(Suc m) = n==m


instance Ord Natural where
	Cero<=_=True
	_<=Cero=False
	(Suc n)<=(Suc m) = n<=m

	
--monomio
data Monomio=M(Int,Int)
data Polinomio = P[Monomio] 

instance Show Monomio where
	show(M(coef,expo))= show coef ++ if(expo==0)then "" else ("X^"++show expo)
	
instance Show Polinomio where
	show(P[])=""
	show (P[m])=show m
	show (P(m:ms))=show m ++ " + " ++ show (P ms)

instance Eq Monomio where
	M(coef1,expo1)==M(coef2,expo2)=(coef1==coef2)&&(expo1==expo2)

instance Eq Polinomio where
	(P[])==(P[])=True
	(P[])==(_)=False
	(_)==(P[])=False
	(P(m1:ms1))==(P(m2:ms2))=(m1==m2)&&((P ms1)==(P ms2))

instance Ord Monomio where
	M(coef1,expo1)<=M(coef2,expo2)=(expo1<=expo2)||((expo1==expo2) && (coef1<=coef2))
	
instance Ord Polinomio where
	(P(m1:ms1))<=(P(m2:ms2))=(m1<=m2)||((m1==m2)&&(P ms1<=P ms2))
	
--Definir una clase Joinable para unir dos valores del mismo tipo
--implementar esta clase para el tipo lista estandar de haskell
--asumiendo que la union de dos listas es simplemente su concatenacion
--Implementar esta clase para el tipo Arbol, definido tal como se indica
--La unidon de dos arboles consite en hacer colgar uno de los arboles de la rama mas a la izquieda
class Joinable c where --Representa al dato instanciado
	join:: c a->c a->c a --la a representa el dato en cuestion
instance Joinable[]where
	join l1 l2=l1 ++ l2
instance Joinable Arbol where
	join Vacio arbol = arbol
	join arbol Vacio = arbol
	join (Nodo n izq der)arbol = Nodo n (join izq arbol) der 
	
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
	
instance Coleccion Cola where
	esVacia(Col c)=length c==0
	insertar(Col c) e=Pil(c++[3])
	primero(Col c)=head c
	eliminar(Col (c:cs))= Col cs
	size (Col c)=length c
	
data Piso = Piso {direccion::String, distancia::Float, precio::Float, plazas::Capacidad}
data Capacidad = C{individuales::Float, dobles::Float}

instance Eq Piso where
	p1==p2 = (precio p1/(individuales(plazas p1)))==(precio p2/(individuales(plazas p2)))

instance Ord Piso where
	p1<= p2 =  (precio p1/(individuales(plazas p1)+ 2* dobles (plazas p1)))>(precio p2/(individuales(plazas p2)+2* dobles (plazas p1)))||( (precio p1/(individuales(plazas p1)+2* dobles (plazas p1)))==(precio p2/(individuales(plazas p2)+2* dobles (plazas p1))) && (distancia p2<= distancia p1))

instance Show Piso where
	show pis = "Piso situado en C/"++ direccion pis ++ " a "++show (distancia pis)++ " km de la Uni" ++ "..."
	
--Ejercicio Restaurante

type MesasLibres=[Mesa]--Lista de mesas libres
type MesasOcupadas=[Mesa]--Lista de mesas ocupadas
data Mesa= Mesa Integer Integer--Numero de mesa y capacidad
data Ocupacion= O MesasLibres MesasOcupadas --Numero de mesas libres y numero de mesas ocupadas

instance Eq Mesa where
	Mesa numero asientos == Mesa numero2 asientos2 = (numero==numero2)&&(asientos==asientos2) --Dos mesas son iguales si coincide su numero de mesa y su cantidad de personas

instance Ord Mesa where -- Las mesas se ordenan segun el numero de asientos que tengan. Su numero de identificacion es  irrelevante
	Mesa numero asientos >= Mesa numero2 asientos2 = asientos>=asientos2
	Mesa numero asientos <= Mesa numero2 asientos2 = asientos<=asientos2
	Mesa numero asientos < Mesa numero2 asientos2 = asientos<asientos2
	Mesa numero asientos > Mesa numero2 asientos2 = asientos>asientos2

instance Show Ocupacion where
	show(O libres ocupadas)= "Mesas libres "+ show libres ++ "Mesas ocupadas "++show ocupadas
	
instance Show Mesa where
	show(Mesa numero asientos)="La mesa "++ show numero ++ "tiene tantos " ++ asientos
	

insertarMesaLibre:: Ocupacion -> Mesa -> Ocupacion
insertarMesaLibre (O libres ocupadas) mesa = O(ordenacion libres mesa)ocupadas

ordenacion::[Mesa]->Mesa->[Mesa]
ordenacion [] mesa=[mesa]
ordenacion (m:ms)mesa= if (m>=mesa)then mesa:m:ms else m:ordenacion ms mesa
	
ocuparMesa:: Ocupacion->Integer->Ocupacion
ocuparMesa ocupacion clientes = buscarMesa ocupacion clientes []

buscarMesa:: Ocupacion->Integer->[Mesa]->Ocupacion
buscarMesa (O(m@(Mesa numero asientos):mesas)ocupadas)clientes otraMesa	= if (asientos >= clientes)then (O(otraMesa ++ mesas)(m:ocupadas))
																			else buscarMesa(O mesas ocupadas)clientes(otraMesa ++[m])