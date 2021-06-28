
module EjerciciosLibro where
import Data.Char
import Data.List

{-Factorial de un numero-}
--Condicional
factorialCondicional::Int->Int
factorialCondicional n = if n == 0 then 1 else n * factorialCondicional (n-1)
--Guardas
factorialGuarda::Int->Int
factorialGuarda n
	|n == 0 = 1
	|otherwise = n * factorialGuarda (n-1)
--Mediante patrones
factorialPatrones::Int->Int
factorialPatrones 0 = 1
factorialPatrones (n+1)= n * factorialPatrones n
--Predefinida
factorialPredefinida::Int->Int
factorialPredefinida n = product[1..]
--Plegado
factorialPlegado::Int->Int
factorialPlegado n = foldr (*) 1 [1..n]

{-Numero de combinacion n m = n!/k!*(n-k)!-}
numeroCombinaciones::Int->Int->Int
numeroCombinaciones n k = (factorialPlegado n) `div` ((factorialPlegado k)*(factorialPlegado(n-k)))