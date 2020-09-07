--Gomez de la Torre Heidi Lizbeth Práctica 2

--Función disyunción
disyuncion::Bool -> Bool -> Bool
disyuncion a b = True
disyuncion False False = False

--Función conjunción
conjuncion:: Bool -> Bool -> Bool
conjuncion True True = True
conjuncion a b = False

--Función implicación
implicacion:: Bool -> Bool -> Bool
implicacion True False = False
implicacion a b = True


--Función bicondicional
dobleImplica:: Bool -> Bool -> Bool
dobleImplica True True = True
dobleImplica a b = False
dobleImplica False False = True

--Función longitud
longitud:: Num a => [a] -> Int
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud (xs)

--Función suma
sumaNumeros:: Num a => [a] -> a
sumaNumeros [] = 0
sumaNumeros [x] = x
sumaNumeros (x:xs) = x + sumaNumeros (xs)

--Máximo en una lista
maximo::Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = if x > maximo xs
					then x
					else maximo xs

--Lugar del elemento
indiceDe:: Num a => Int -> [a] -> a
indiceDe i (x:xs) = if i < 0 || i > longitud (x:xs)
										then error "El elemento que buscas no está en la lista"
										else if i == 0
												then x
												else indiceDe (i-1) xs

--Insertar elemento
insertarElemento:: Eq a => a -> [a] -> Bool -> [a]
insertarElemento a xs True = (a:xs)
insertarElemento a xs False = (xs) ++ [a]

--Reversa
reversa:: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa (xs) ++ [x]

--Buscar una x
-- listas ligadas
buscarX :: Eq a => a -> [a] -> Bool
buscarX elem [] = False
buscarX elem (x:xs) = if elem == x
									then True
									else buscarX elem xs

--Convertir en conjunto
aConjunto:: Eq a => [a] -> [a]
aConjunto [] = []
aConjunto (x:xs) = if buscarX x xs
									then aConjunto xs
									else aConjunto xs ++ [x]

--Union de conjuntos
union:: Eq a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) = aConjunto ((x:xs)++(y:ys))

--Intesercción de conjuntos
interseccion:: Eq a => [a] -> [a] -> [a]
interseccion xs [] = []
interseccion [] ys = []
interseccion (x:xs) (y:ys) = if buscarX x (y:ys)
														then aConjunto ([x] ++ interseccion xs (y:ys))
														else interseccion xs (y:ys)

--Producto cartesiano
productoCruz:: Ord a => [a] -> [a] -> [(a,a)]
productoCruz [] [] = []
productoCruz xs ys = [ (x,y) | x <- xs, y <- ys ]

--Diferencia
diferencia :: Ord a => [a]-> [a] -> [a]
diferencia (x:xs) ys = if buscarX x ys
											then diferencia xs ys
											else diferencia xs ys ++ [x]

--Diferencia simetrica
diferenciaSimetrica:: Ord a => [a] -> [a] -> [a]
diferenciaSimetrica xs [] = xs
diferenciaSimetrica [] xs = []
diferenciaSimetrica xs ys = diferencia (union xs ys) (interseccion xs ys)

--Divisible
divisible:: Int -> Int -> Bool
divisible a b = a `mod` b == 0

--Divisores de un número
divisores:: Int -> [Int]
divisores x = [ y | y <- [1..x], divisible x y ]
