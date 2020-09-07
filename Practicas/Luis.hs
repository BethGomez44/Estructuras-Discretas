graficasConexasM :: Grafica -> [[Integer]]
graficasConexasM (v,[]) = []
graficasConexasM (v,((a,b,c):xs))  = [graficaConexa [a] [] (v,(a,b,c))]

graficaConexa :: [Integer] -> [Integer] -> Grafica -> [Integer]
graficaConexa [] xs g = xs
graficaConexa (y:ys) xs g = if buscarX y xs
                            then graficaConexa ys xs
                            else graficaConexa (ingrado y g) ++ (exgrado y g) ++ (y:ys) g

eliminarAdy :: Integer -> [(Integer,Integer,Float)]
eliminarAdy n [] = []
eliminarAdy n ((a,b,c):xs) = if n == a || n == b
                             then eliminarAdy n xs
                             else ((a,b,c):(eliminarAdy n xs))

eliminarAdyM :: [Integer] -> [(Integer,Integer,Float)]
eliminarAdyM [] xs = xs
eliminarAdyM (y:ys) xs = eliminarAdyM ys (eliminarAdy y xs)