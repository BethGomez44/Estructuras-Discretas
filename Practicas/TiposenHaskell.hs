--Tipos en haskell

--Definicion de un tipo
data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Show,Eq,Ord)
data Formula = Prop Var | Neg Formula
              | Formula :&: Formula
              | Formula :|: Formula
              | Formula :=>: Formula
              | Formula :<=>: Formula
              deriving (Show, Eq, Ord)

--Tomaremos a fs como una lista cualquiera.

--    Ejemplo :
--      1. p -> q en Tipo Haskell sería: Prop p :=>: Prop Q

--      2. (p -> q) ^ z -> [p,q,z]
--        En Haskell:
varList:: Formula -> [Var]
varList (Prop x) = [x]
varList (Neg fs) = (varList fs)
varList (ps :&: qs) = (varList ps) ++ (varList qs)
varList (ps :|: qs) = (varList ps) ++ (varList qs)
varList (ps :=>: qs) = (varList ps) ++ (varList qs)
varList (ps :<=>: qs) = (varList ps) ++ (varList qs)
--(varList (Prop p :=>: Neg((Prop Q :<=>: Prop W) :&: Neg (Prop p))) = varList (Prop p) ++ varList (Neg (Prop Q :<=>: Prop W) :&: Neg (Prop p)))
--infix = Orden de precedencia
infixl 9 :&:
infixl 9 :|:
infixr 8 :=>:
infixl 7 :<=>:
