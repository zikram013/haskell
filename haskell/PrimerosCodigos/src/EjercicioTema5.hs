
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