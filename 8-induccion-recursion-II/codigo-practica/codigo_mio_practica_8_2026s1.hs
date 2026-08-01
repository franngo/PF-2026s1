--Sección I. Ejercicio 1.

{- 
esquema de recursión sobre la estructura de lista:
funcion []     = ...
funcion (x:xs) = ... x ... Main.funcion xs
-}

--a. length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + Main.length xs 

--b. sum :: [Int] -> Int
sum []     = 0
sum (n:ns) = n + Main.sum ns

--c. product :: [Int] -> Int
product []     = 1
product (n:ns) = n * Main.product ns

--d. concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ Main.concat xss

--e. elem :: Eq a => a -> [a] -> Bool
elem x []     = False
elem x (y:ys) = x==y || Main.elem x ys

--f. all :: (a -> Bool) -> [a] -> Bool
all f []     = True
all f (x:xs) = f x && Main.all f xs

--g. any :: (a -> Bool) -> [a] -> Bool
any f []     = False
any f (x:xs) = f x || Main.any f xs

--h. count :: (a -> Bool) -> [a] -> Int
count f []     = 0
count f (x:xs) = unoSi f x + Main.count f xs

unoSi f x = if f x
                then 1
                else 0

--i. subset :: Eq a => [a] -> [a] -> Bool
subset []     ys = True
subset (x:xs) ys = Main.elem x ys && Main.subset xs ys

--j. (++) :: [a] -> [a] -> [a]
--[]     ++ ys = ys
--(x:xs) ++ ys = x : (xs ++ ys)

--Le cambié el nombre porque me chocaba con la definición de Hugs.Prelude. También la hice prefija porque me daba error sino
masmas []     ys = ys
masmas (x:xs) ys = x : masmas xs ys

--k. reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = Main.reverse xs ++ [x]

--Otra versión medio falopa usando "eliminación de recursión usando acumuladores". No estoy seguro si es RE ¿Creo que sí?
--Mucho más eficiente, porque no usa un (++) por cada elemento de la lista, sino uno (:), que es O(1)
reverse' :: [a] -> [a]
reverse' xs = rev xs []

rev :: [a] -> [a] -> [a]
rev []     acu = acu
rev (x:xs) acu = rev xs (x:acu)


--l. zip :: [a] -> [b] -> [(a,b)]
--Solo lo veo posible recorriendo ambas listas a la vez, ya que estas pueden ser de distinta longitud.
--Quizás otra opción podría ser recursión primitiva y preguntando con un if si tail ys es null (y usando head ys para formar el par)
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : Main.zip xs ys

--esta es otra forma más clara (pero no usa RE!! Fidel dijo que es una definición FEA. Repercute a la hora de foldear)
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _      _      = []


--m. unzip :: [(a,b)] -> ([a],[b])
unzip []     = ([], [])
unzip (p:ps) = agregarAListas p (Main.unzip ps)

agregarAListas (x,y) (xs, ys) = (x : xs, y : ys)

{- 
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
-}

--Sección II. Ejercicio 1.a.

{- 
esquema de recursión sobre la estructura de N:
funcion Z     = ...
funcion (S n) = ... S ... funcion n
-}

data N = Z | S N
    deriving Show

cero = Z
uno = S Z
dos = S (S Z)
tres = S (S (S Z))
cuatro = S (S (S (S Z)))
cinco = S (S (S (S (S Z))))

--evalN :: N -> Int
evalN Z     = 0
evalN (S n) = 1 + evalN n

--addN :: N -> N -> N
addN Z     n' = n'
addN (S n) n' = S (addN n n')

--prodN :: N -> N -> N
prodN Z     n' = Z
prodN (S n) n' = addN n' (prodN n n')

--int2N :: Int -> N
int2N 0 = Z
int2N n = S (int2N (n-1))

{- 
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
-}

--Sección II. Ejercicio 2.a.

{- 
esquema de recursión sobre la estructura de NU:
funcion []     = ...
funcion (u:us) = ... u ... funcion us
-}

type NU = [()]

cero' = []
uno' = [()]
dos' = [(),()]
tres' = [(),(),()]
cuatro' = [(),(),(),()]
cinco' = [(),(),(),(),()]

------
evalNU :: NU -> Int
evalNU []     = 0
evalNU (u:us) = 1 + evalNU us

------
succNU :: NU -> NU
succNU us = () : us

------
addNU :: NU -> NU -> NU
addNU []     us' = us'
addNU (u:us) us' = u : addNU us us'

------
nu2n :: NU -> N
nu2n []     = Z
nu2n (u:us) = S (nu2n us)

------
n2nu :: N -> NU
n2nu Z     = []
n2nu (S n) = () : n2nu n

{- 
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
-}

--Sección II. Ejercicio 3.a.

--Tipo y funciones necesarias (definidas en práctica 5)

data DigBin = O | I
    deriving Show

dbAsInt:: DigBin -> Int
dbAsInt O = 0
dbAsInt I = 1

dbAsBool:: DigBin -> Bool
dbAsBool O = False
dbAsBool I = True

dbOfBool:: Bool -> DigBin
dbOfBool False = O
dbOfBool True = I

negDB:: DigBin -> DigBin
negDB O = I
negDB I = O

----------------------------

{- 
esquema de recursión sobre la estructura de NBin:
funcion []     = ...
funcion (d:ds) = ... d ... funcion ds
-}

type NBin = [DigBin]

cero'' = [O]
uno'' = [I]
dos'' = [O,I]
tres'' = [I,I]
cuatro'' = [O,O,I]
cinco'' = [I,O,I]
seis'' = [O,I,I]
siete'' = [I,I,I]

--Se asume que [] es equivalente a [O]

------
evalNB :: NBin -> Int
evalNB []     = 0
evalNB (d:ds) = dbAsInt d + 2 * evalNB ds

------
succNB :: NBin -> NBin
succNB []     = [I]
succNB (O:ds) = I : ds
succNB (I:ds) = O : succNB ds
--rompemos el esquema de rec. estructural, pero no nos queda el if o el case que es molesto para las demos.

succNB' :: NBin -> NBin
succNB' []     = [I]
succNB' (d:ds) = case d of
                    O -> I : ds
                    I -> O : succNB' ds
--lo que tiene de malo es el case (por el tema demos)


------
--Versión que me salió sin usar las subtareas sugeridas. Es igual a la que hizo Fede en la práctica. Es cara porque es cuadrática.
--Con las subtareas sugeridas era complicadito hacerlo a menos (addDBConCarry)
--Fede tmb se aventuró a hacer esa versión, pero creo que le cambió un poco el tipo.
addNB :: NBin -> NBin -> NBin
addNB []     []       = [O]
addNB []     ds'      = ds'
addNB ds     []       = ds
addNB (d:ds) (d':ds') = addDB d d' (addNB ds ds')

addDB :: DigBin -> DigBin -> NBin -> NBin
addDB O O ds = O : ds 
addDB O I ds = I : ds
addDB I O ds = I : ds
addDB I I ds = O : succNB ds

------
normalizarNB :: NBin -> NBin
normalizarNB []     = []
normalizarNB (d:ds) = normDB d (normalizarNB ds)

normDB :: DigBin -> NBin -> NBin
normDB O [] = []
normDB I [] = I:[]
normDB d ds = d:ds

--Realmente el caso con I [] se podría haber obviado, porque ya lo cubrimos en el de abajo, donde agregamos el dígito a la lista.
normDB' :: DigBin -> NBin -> NBin
normDB' O [] = []
normDB' d ds = d:ds

------
--primera versión que se me ocurrió, que no involucra recursión estructural. Es O(n*2)
nb2n' :: NBin -> N
nb2n' ds = int2N (evalNB ds)

--segunda versión con recursión. Muy cara. Diría que cercana a cuadrática por el prodN que hace un addN, que es O(x), siendo x en este caso 
--la longitud del N de la recursión (pero coincide con la que hizo Fede en la pŕactica)
nb2n :: NBin -> N
nb2n []     = Z
nb2n (d:ds) =  addN (int2N (dbAsInt d))  (prodN (S (S Z)) (nb2n ds))

------
--es cuadrática
n2nb :: N -> NBin
n2nb Z     = []
n2nb (S n) = succNB (n2nb n)

{- 
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
-}

--Sección III. Ejercicio 1.a.

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show

{- 
esquema de recursión sobre la estructura de NU:
funcion (Cte n)          = ... n
funcion (Suma exp1 exp2) = ...  ... funcion exp1 ... funcion exp2
funcion (Prod exp1 exp2) = ...  ... funcion exp1 ... funcion exp2
-}

c0 = Cte 0
c1 = Cte 1
c2 = Cte 2
c3 = Cte 3
c4 = Cte 4
c5 = Cte 5
c6 = Cte 6
sum23 = Suma c2 c3
sum45 = Suma c4 c5
prod23 = Prod c2 c3
prod45 = Prod c4 c5

sumc0 = Suma c0 prod23
sumc0' = Prod (Suma (Cte 4) (Cte 0)) (Suma (Cte 3) (Cte 9))
prodc0 = Prod c0 prod23
prodc0' = Prod (Prod (Cte 4) (Cte 0)) (Prod (Cte 3) (Cte 9))
prodc0'' = Suma (Prod (Cte 4) (Cte 0)) (Prod (Cte 3) (Cte 9))
prodc1 = Prod c1 prod23
prodc1' = Prod (Prod (Cte 4) (Cte 1)) (Prod (Cte 3) (Cte 9))
tresSumaCero = Suma (Suma (Suma (Cte 0) (Cte 77)) (Cte 0)) (Suma (Cte 0) (Cte 9))

------
evalExpA :: ExpA -> Int
evalExpA (Cte n)          = n
evalExpA (Suma exp1 exp2) = evalExpA exp1 + evalExpA exp2
evalExpA (Prod exp1 exp2) = evalExpA exp1 * evalExpA exp2

------
simplificarExpA :: ExpA -> ExpA
simplificarExpA (Cte n)          = Cte n
simplificarExpA (Suma exp1 exp2) = eliminarSumaRedundante (simplificarExpA exp1) (simplificarExpA exp2)
simplificarExpA (Prod exp1 exp2) = eliminarProdRedundante (simplificarExpA exp1) (simplificarExpA exp2)

eliminarSumaRedundante :: ExpA -> ExpA -> ExpA
eliminarSumaRedundante (Cte 0) exp2 = exp2
eliminarSumaRedundante exp1 (Cte 0) = exp1
eliminarSumaRedundante exp1 exp2    = Suma exp1 exp2

eliminarProdRedundante :: ExpA -> ExpA -> ExpA
eliminarProdRedundante (Cte 0) exp2 = Cte 0
eliminarProdRedundante exp1 (Cte 0) = Cte 0
eliminarProdRedundante (Cte 1) exp2 = exp2
eliminarProdRedundante exp1 (Cte 1) = exp1
eliminarProdRedundante exp1 exp2    = Prod exp1 exp2

------
cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n)          = 0
cantidadDeSumaCero (Suma exp1 exp2) = unoSiSumaCero exp1 exp2 + cantidadDeSumaCero exp1 + cantidadDeSumaCero exp2
cantidadDeSumaCero (Prod exp1 exp2) = cantidadDeSumaCero exp1 + cantidadDeSumaCero exp2

unoSiSumaCero :: ExpA -> ExpA -> Int
unoSiSumaCero (Cte 0) _ = 1
unoSiSumaCero _ (Cte 0) = 1
unoSiSumaCero _ _       = 0

{- 
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
========================================================================================
-}

--Lo de ExpS quedó sin hacer
