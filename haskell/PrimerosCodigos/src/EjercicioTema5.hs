
module EjercicioTema5 where
--pila

data PilaInt= Vacia | Cima Int PilaInt
p = Cima 7 (Cima 4 (Cima 0 Vacia))

--Ejercicio racional
--nombre del dato,constructor,
data Racional = R Int Int deriving Show
equivalentes::Racional->[Racional]->[Racional]
equivalentes_[]=[]
equivalentes(R num den)((R num1 den1):rs)=if(num*den1==num1*den)then (R num1 den1):(equivalentes (R num den)rs)else(equivalentes (R num den)rs)

--Ejercicio de arboles binarios
data Arbol a=AV| Nodo a(Arbol a)(Arbol a) deriving Show
numNodos :: Arbol a -> Int
numNodos AV = 0
numNodos (Nodo r izq der) = 1 + numNodos izq + numNodos der

numHojas ::Arbol a->Int
numHojas  AV = 0
numHojas (Nodo _ AV AV)=1
numHojas (Nodo _ izq der)=numHojas izq + numHojas der

altura::Arbol a-> Int
altura AV=0
altura (Nodo a iz der)=1 + tamanio(altura(iz),altura(der))

tamanio::(Int,Int)->Int
tamanio (a,b)=if a>b then a else b

--recorrido inorden izq,raiz,der
inorden::Arbol a ->[a]
inorden AV = []
inorden (Nodo r izq der) = inorden izq ++[r]++ inorden der

postorden:: Arbol a->[a]
postorden AV=[]
postorden (Nodo r izq der)=postorden izq ++ postorden der ++ [r]

preorden:: Arbol a->[a]
preorden AV=[]
preorden(Nodo r izq der)= [r]++preorden izq ++ preorden der 

--Estan dos nodos al mismo nivel
--nivelNodo::(Eq a,Ord a) =>Arbol a ->[a]->Int
--nivelNodo AV _=0
--nivelNodo (Nodo r izq der) x= if(r==x)then 1 
								--else aux + if(aux==0)then 0
											--else 1 where aux=max(nivelNodo izq x, nivelNodo der x)
											
--nodos hermanos
hermanos::Eq a =>(Arbol a,a,a)->Bool
hermanos (AV ,_ ,_ )= False
hermanos (Nodo r AV AV,_,_)=False
hermanos(Nodo r AV (Nodo rhd deri derd),h1,h2)=hermanos((Nodo rhd deri derd),h1,h2)
hermanos(Nodo r(Nodo rhi izi izd)AV,h1,h2)=hermanos((Nodo rhi izi izd),h1,h2)
hermanos(Nodo r(Nodo rhi izi izd)(Nodo rhd deri derd),h1,h2)
	|(rhi==h1)&&(rhd==h2)=True
	|(rhi==h2)&&(rhd==h1)=True
	|otherwise=hermanos(Nodo rhi izi izd,h1,h2)||hermanos(Nodo rhd deri derd, h1,h2)
	
--Reconstruir arbol
reconstruir::(Eq a)=>[a]->[a]->Arbol a
reconstruir [ ] [ ] = AV
reconstruir (p:ps) inorden = Nodo p (reconstruir pri ini ) (reconstruir prd ind )
                                           where
                                             (ini,ind) = partirL inorden p [ ]
                                             (pri,prd) =partirD ps ( length ini) [ ]
                                                 
partirL::(Eq a)=> [a]->a->[a]->([a],[a])
partirL (x:xs) p acum = if x==p then (acum,xs)  else partirL xs p (acum++[x]) 	


partirD::[a]->Int->[a]->([a],[a])
partir[]_ acum=(acum,[])
partirD (x:xs) elementos acum = if elementos==0 then (acum,x:xs)
                                   else partirD xs (elementos-1) (acum++[x])