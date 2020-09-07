--Gomez de la Torre Heidi Lizbeth
--Práctica 3

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving ( Show, Eq, Ord)
data Formula = Prop Var|Neg Formula|Formula :&: Formula|Formula :|: Formula|Formula :=>: Formula|Formula :<=>: Formula deriving (Show, Eq, Ord)
infixl 9 :&:
infixl 9 :|:
infixl 7 :=>:
infixl 8 :<=>:

--Función variables de una formula
varList:: Formula -> [Var]
varList xs = aConjunto (auxVarList (xs))

--Función Negación
negar:: Formula -> Formula
negar (Prop x) = Neg (Prop x)
negar (Neg xs) = xs
negar (xs :&: ys) = (negar xs) :|: (negar ys)
negar (xs :|: ys) = (negar xs) :&: (negar ys)
negar (xs :=>: ys) = (xs) :&: (negar ys)
negar (xs :<=>: ys) = ((xs) :&: (negar ys)) :|: ((ys) :&: (negar xs)) 

--Función Equivalencia
equivalencia:: Formula -> Formula
equivalencia (Prop x) = Prop x
equivalencia (Neg xs) = negar (equivalencia xs)
equivalencia (xs :&: ys) = (equivalencia ys) :&: (equivalencia xs)
equivalencia (xs :|: ys) = (equivalencia xs) :|: (equivalencia ys)
equivalencia (xs :=>: ys) = (negar xs) :|: (equivalencia ys)
equivalencia (xs :<=>: ys) = equivalencia(xs :=>: ys) :&: equivalencia (ys :=>: xs)

--Función Interpretación
interp:: Formula -> [(Var,Bool)] -> Bool
interp (Prop x) [] = error "No todas las variables estan definidas"
interp (Prop x) ((y,b):xs) = if x == y
							then b
							else interp (Prop x) xs
interp (Neg (Prop x)) zs = no (interp (Prop x) zs)
interp (xs :&: ys) zs = (interp xs zs) && (interp ys zs)
interp (xs :|: ys) zs = (interp xs zs) || (interp ys zs)
interp (xs :=>: ys) zs = if (interp xs zs) == True && (interp ys zs) == False
							then False
							else True
interp (xs :<=>: ys) zs = if (interp xs zs) /= (interp ys zs)
							then False
							else True

--Función Combinaciones
combinaciones:: Formula -> [[(Var,Bool)]]
combinaciones xs = asignarInterp (varList xs) (auxCombin ((longitud (varList xs))-1) [[False],[True]])

--Función Tabla de Verdad
tablaVerdad:: Formula -> [([(Var,Bool)],Bool)]
tablaVerdad ys = auxTabla ys (combinaciones ys)

--Funciones auxiliares

-- Aux. Variables
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

longitud:: [a] -> Int
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud (xs)

auxVarList:: Formula -> [Var]
auxVarList (Prop x) = [x]
auxVarList (Neg xs) = (auxVarList xs)
auxVarList (xs :&: ys) = (auxVarList xs) ++ (auxVarList ys)
auxVarList (xs :|: ys) = (auxVarList xs) ++ (auxVarList ys)
auxVarList (xs :=>: ys) = (auxVarList xs) ++ (auxVarList ys)
auxVarList (xs :<=>: ys) = (auxVarList xs) ++ (auxVarList ys)

--Aux. Equivalencia
no:: Bool -> Bool
no True = False
no False = True

--Aux. Combinaciones
combina:: [[Bool]] -> Bool -> [[Bool]]
combina [] x = []
combina (x:xs) b = ((b:x):(combina xs b))

auxCombin:: Int -> [[Bool]] -> [[Bool]]
auxCombin 0 xs = xs
auxCombin n xs = auxCombin (n-1) ((combina xs False) ++ (combina xs True))

aValor:: [Var] -> [Bool] -> [(Var,Bool)]
aValor [] [] = []
aValor (x:xs) (y:ys) = (x,y):aValor xs ys

asignarInterp:: [Var] -> [[Bool]] -> [[(Var,Bool)]]
asignarInterp [] [] = []
asignarInterp xs [] = []
asignarInterp xs (y:ys) = (aValor xs y:asignarInterp xs ys)

--Aux. Tabla de Verdad
auxTabla:: Formula -> [[(Var,Bool)]] -> [([(Var,Bool)],Bool)]
auxTabla ys [] = []
auxTabla ys (x:xs) = ((x),(interp ys x)):(auxTabla ys xs)