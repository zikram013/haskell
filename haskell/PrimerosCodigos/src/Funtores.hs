
module Funtores where

--Un funtor es una clase a la que estan instanciadas (y/o se pueden instanciar)
--todos los constructores de tipos que pueden ser mapeados: listas, IO, Maybe,funciones

--class Functor f where
	--fmap::(a->b)->f a-> f b
	--(<$):: a->f b -> f a
	
--instance Functor [ ]where
	--fmap function l = map function l
	--(<$) x [ ]=[ ]
	--(<$) x (l:ls)=x:( (<$) x ls) --sustituye por x todos los elementos de la lista
	--(<$) 3 [] cambia a tres toda la lista
	--(3*)<$>[]multiplica por tres toda la lista
	--((3*)<$>(5+))7 hace 5 + 7 y le multiplica 3
	--fmap (3*)(5+)7 haria lo mismo
data Arbol a=Nodo a (Arbol a)(Arbol a) | Vacio deriving (Show)	
a1 = Nodo 3 (Nodo 5 (Nodo (-1) Vacio Vacio) (Nodo 7 Vacio (Nodo 6 Vacio Vacio)))(Nodo 2 Vacio Vacio)

instance Functor Arbol where
	fmap f Vacio=Vacio
	fmap f (Nodo r izq der)=Nodo(f r)(fmap f izq)(fmap f der)
	(<$) v Vacio = Vacio
	(<$) v (Nodo r izq der)=Nodo v((<$)v izq)((<$)v der)
	
--Funtores Aplicativos
--Un paso mas alla son los funtores aplicativos, que permiten, por ejemplo, aplicar una lista
--de funciones sobre una lista de valores gracias a las operacion <*>

--ejemplo
divisores:: Int ->[Int]
divisores 0=[]
divisores n=[x| x<-[1..n],n `rem`x==0]
