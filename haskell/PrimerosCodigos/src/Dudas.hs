
module Dudas where
data Arbol a=AV| Nodo a(Arbol a)(Arbol a) deriving Show

nivelNodo::(Eq a,Ord a) =>Arbol a ->[a]->Int
nivelNodo AV _=0
nivelNodo (Nodo r izq der) x= if(r==x)then 1 
								else aux + if(aux==0)then 0
											else 1 where aux=max(nivelNodo izq x, nivelNodo der x)
											
maxi::(Int,Int)->Int
maxi (a,b)=if a>b then a else b										