--Gomez de la Torre Heidi Lizbeth
--Práctica 4

type Grafica = ([Integer],[(Integer,Integer,Float)])

--Ejercicios de Graficas

--Función Incidencias
incidencias :: Grafica -> Integer -> [Integer]
incidencias (xs,[]) n = []
incidencias (xs,((a,b,c):ys)) n = if n == b
                                  then [a] ++ incidencias (xs,ys) n
                                  else incidencias (xs,ys) n
--Función Adyacencias
adyacencias :: Grafica -> Integer -> [Integer]
adyacencias (xs,[]) n = []
adyacencias (xs,((a,b,c):ys)) n = if n == a
                                  then [b] ++ adyacencias (xs,ys) n
                                  else adyacencias (xs,ys) n

--Función Ingrado
ingrado :: Grafica -> Integer -> Integer
ingrado (xs,[]) n = 0
ingrado (xs,(a,b,c):ys) n = if n == b
                            then 1 + ingrado (xs,ys) n
                            else ingrado (xs,ys) n

--Función Exgrado
exgrado :: Grafica -> Integer -> Integer
exgrado (xs,[]) n = 0
exgrado (xs,(a,b,c):ys) n = if n == a
                            then 1 + exgrado (xs,ys) n
                            else exgrado (xs,ys) n

--Función Kruskal
--kruskal :: Grafica -> Grafica
--kruskal 

--Función Halmitoniana
--esHamiltoniana :: Grafica -> Bool

--Ejercicios de Relaciones y Funciones

--Función Dominio
dominio :: Grafica -> [Integer]
dominio (xs,[]) = []
dominio (xs,(a,b,c):ys) = [a] ++ dominio (xs,ys)

--Función Imagen
imagen :: Grafica -> [Integer]
imagen (xs,[]) = []
imagen (xs,(a,b,c):ys) = [b] ++ imagen (xs,ys)

--Función Verificadora
esFuncion :: Grafica -> Bool
esFuncion (xs,[]) = False
esFuncion g = if [longitud (dominio g)] /= aConjunto (dominio g)
              then False
              else True 

--Función Reflexiva
esReflexiva :: Grafica -> Bool
esReflexiva ([],ys) = True
esReflexiva ((x:xs),ys) = if buscarPar (x,x) (xs,ys)
                              then esReflexiva (xs,ys)
                              else False

--Función Simétrica
esSimetrica :: Grafica -> Bool
esSimetrica (xs,[]) = True
esSimetrica (xs,(a,b,c):ys) = if buscarPar (b,a) (xs,ys) 
                              then esSimetrica (xs,(eliminar (b,a,c) ys)) 
                              else False

--Función Antisimétrica
esAntisimetrica :: Grafica -> Bool
esAntisimetrica (xs,[]) = True
esAntisimetrica (xs,(a,b,c):ys) = if not (buscarPar (b,a) (xs,ys)) || a == b 
                                  then esAntisimetrica (xs,(eliminar (b,a,c) ys)) 
                                  else False

--Función Composición
composicion :: Grafica -> Grafica -> Grafica
composicion (zs,ws) (xs,ys) = (vertices (composicionAux (zs,ws) (xs,ys)), composicionAux (zs,ws) (xs,ys))

--Función Potencia
potencia :: Grafica -> Integer -> Grafica
potencia g 0 = g
potencia g n = potencia (composicion g g) (n-1)

--Función Cerradura Reflexiva
cerrReflexiva :: Grafica -> Grafica
cerrReflexiva ([],ys) = ([],[])
cerrReflexiva (xs,(a,b,c):ys) = if esReflexiva (xs,(a,b,c):ys) == False
                                then (xs,union ((a,b,c):ys) [(a,a,c) | a <- xs]) 
                                else (xs,(a,b,c):ys)

--Función Cerradura Simétrica
cerrSimetrica :: Grafica -> Grafica
cerrSimetrica ([],ys) = ([],[])
cerrSimetrica (xs,(a,b,c):ys) = if esSimetrica (xs,(a,b,c):ys) == False
                                then (xs,union ((a,b,c):ys) [(b,a,c)|(a,b,c) <- (a,b,c):ys]) 
                                else (xs,(a,b,c):ys)

--Función Cerradura Transitiva
--cerrTransitiva :: Grafica -> Grafica

--Auxiliares de Graficas


--Auxiliares de Relaciones y Funciones

buscarX :: Eq a => a -> [a] -> Bool
buscarX elem [] = False
buscarX elem (x:xs) = if elem == x
						then True
                        else buscarX elem xs
                        
aConjunto:: Eq a => [a] -> [a]
aConjunto [] = []
aConjunto (x:xs) = if buscarX x xs
					then aConjunto xs
                    else aConjunto xs ++ [x]

longitud:: Num a => [a] -> Integer
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud (xs)

buscarPar :: (Integer,Integer) -> Grafica -> Bool
buscarPar (x,y) (xs,[]) = False
buscarPar (x,y) (xs,(a,b,c):ys) = if x == a && y == b
                                  then True
                                  else buscarPar (x,y) (xs,ys)

eliminar :: (Integer,Integer,Float) -> [(Integer,Integer,Float)] -> [(Integer,Integer,Float)]
eliminar x [] = []
eliminar (a,b,c) ((x,y,z):xs) = if x == a && b == y
                                  then eliminar (a,b,c) xs
                                  else ((x,y,z):eliminar (a,b,c) xs)
union:: Eq a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) = aConjunto ((x:xs)++(y:ys))

vertices:: [(Integer,Integer,Float)] -> [Integer]
vertices ys = aConjunto (aConjunto [ x | (x,y,z) <- ys] ++ aConjunto [ y | (x,y,z) <- ys])

composicionAux:: Grafica -> Grafica -> [(Integer,Integer,Float)]
composicionAux (zs,ws) (xs,ys) = aConjunto [(a,p,c+q) | (a,b,c) <- ys, (o,p,q) <- ws, b == o]

