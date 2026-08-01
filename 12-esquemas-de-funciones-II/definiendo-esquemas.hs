--Definición de esquemas de recursión estructural y primitiva para tipos algebraicos recursivos:

--En los esquemas vamos a respetar el orden de los constructores.

--Partimos del tipo algebraico ExpA y de sus 3 constructores
data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show
{-
Cte :: Int -> ExpA
Suma :: ExpA -> ExpA -> ExpA
Prod :: ExpA -> ExpA -> ExpA

Lo que hacemos ahora es remplazar el tipo algebraico recursivo por b

Cte :: Int -> b
Suma :: b -> b -> b
Prod :: b -> b -> b
(además de ExpA -> b)

Estos van a ser los tipos de las funciones que recibe el esquema de recursión estructural sobre la estructura de ExpA (foldExpA).
Además, obviamente, también recibe el tipo algebraico recursivo y devuelve un b.

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b

En los esquemas, vamos a respetar el orden de los constructores respecto del orden en que les pasamos las funciones
correspondientes a cada uno.
-}

foldExpA :: (Int -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA fc fs fp (Cte n)          = fc n
foldExpA fc fs fp (Suma exp1 exp2) = fs (foldExpA fc fs fp exp1) (foldExpA fc fs fp exp2)
foldExpA fc fs fp (Prod exp1 exp2) = fp (foldExpA fc fs fp exp1) (foldExpA fc fs fp exp2)

{-
Para definir el esquema de recursión primitiva sobre la estructura de ExpA, el tipo será el mismo, pero antes de cada b resultante
de la recursión, también va a ir el tipo recursivo (la parte inductiva sobre la cual se hace la recursión)

Cte :: Int -> b
Suma :: ExpA -> b -> ExpA -> b -> b
Prod :: ExpA -> b -> ExpA -> b -> b
(además de ExpA -> b)
-}

recExpA :: (Int -> b) -> (ExpA -> b -> ExpA -> b -> b) -> (ExpA -> b -> ExpA -> b -> b) -> ExpA -> b
recExpA fc fs fp (Cte n)          = fc n
recExpA fc fs fp (Suma exp1 exp2) = fs exp1 (recExpA fc fs fp exp1) exp2 (recExpA fc fs fp exp2)
recExpA fc fs fp (Prod exp1 exp2) = fp exp1 (recExpA fc fs fp exp1) exp2 (recExpA fc fs fp exp2)


-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------

--Foldeando funciones sobre ExpA

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

--Acá b se instancia en Int
evalExpA' :: ExpA -> Int
evalExpA' = foldExpA id (\n1 n2 -> n1 + n2) (\n1 n2 -> n1 * n2)

--O mejor
evalExpA'' :: ExpA -> Int
evalExpA'' = foldExpA id (+) (*)

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

--Acá b se instancia en ExpA
simplificarExpA' :: ExpA -> ExpA
simplificarExpA' = foldExpA (\n -> Cte n) (\exp1 exp2 -> eliminarSumaRedundante exp1 exp2) (\exp1 exp2 -> eliminarProdRedundante exp1 exp2)

------
cantidadDeSumaCero :: ExpA -> Int
cantidadDeSumaCero (Cte n)          = 0
cantidadDeSumaCero (Suma exp1 exp2) = unoSiSumaCero exp1 exp2 + cantidadDeSumaCero exp1 + cantidadDeSumaCero exp2
cantidadDeSumaCero (Prod exp1 exp2) = cantidadDeSumaCero exp1 + cantidadDeSumaCero exp2

unoSiSumaCero :: ExpA -> ExpA -> Int
unoSiSumaCero (Cte 0) _ = 1
unoSiSumaCero _ (Cte 0) = 1
unoSiSumaCero _ _       = 0

--Acá b se instancia en Int
cantidadDeSumaCero' :: ExpA -> Int
cantidadDeSumaCero' = recExpA (const 0) fs fp
    where fs exp1 n1 exp2 n2 = unoSiSumaCero exp1 exp2 + n1 + n2
          fp _    n1 _    n2 = n1 + n2

-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------

--Apuntes de clase práctica (temas de 11 y 12) con el perspicaz Fede:

data Dir = Left | Right | Straight
    deriving Show

data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)   
    deriving Show

foldMapa :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b    
foldMapa fc fn fb (Cofre xs)               = fc xs
foldMapa fc fn fb (Nada mp)                = fn (foldMapa fc fn fb mp)
foldMapa fc fn fb (Bifurcacion xs mp1 mp2) = 
    fb xs (foldMapa fc fn fb mp1) 
          (foldMapa fc fn fb mp2)

          

recMapa :: ([a] -> b) -> (Mapa a -> b -> b) -> ([a] -> Mapa a -> b -> Mapa a -> b -> b) -> Mapa a -> b    
recMapa fc fn fb (Cofre xs)               = fc xs
recMapa fc fn fb (Nada mp)                = fn mp (recMapa fc fn fb mp)
recMapa fc fn fb (Bifurcacion xs mp1 mp2) = 
    fb xs mp1 (recMapa fc fn fb mp1) 
          mp2 (recMapa fc fn fb mp2)

-------------------------------------------------------------------------------------------------------------------------------

data ExpBool = And ExpBool ExpBool | Or ExpBool ExpBool | Not ExpBool | Cnte Bool
    deriving Show

foldEB :: (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> (Bool -> b) -> ExpBool -> b
foldEB fa fo fn fc (And eb1 eb2) = fa (foldEB fa fo fn fc eb1) (foldEB fa fo fn fc eb2)
foldEB fa fo fn fc (Or eb1 eb2)  = fo (foldEB fa fo fn fc eb1) (foldEB fa fo fn fc eb2)
foldEB fa fo fn fc (Not eb)      = fn (foldEB fa fo fn fc eb)
foldEB fa fo fn fc (Cnte b)      = fc b

recEB :: (ExpBool -> b -> ExpBool -> b -> b) -> (ExpBool -> b -> ExpBool -> b -> b) -> (ExpBool -> b -> b) -> (Bool -> b) -> ExpBool -> b
recEB fa fo fn fc (And eb1 eb2) = fa eb1 (recEB fa fo fn fc eb1) eb2 (recEB fa fo fn fc eb2)
recEB fa fo fn fc (Or eb1 eb2)  = fo eb1 (recEB fa fo fn fc eb1) eb2 (recEB fa fo fn fc eb2)
recEB fa fo fn fc (Not eb)      = fn eb (recEB fa fo fn fc eb)
recEB fa fo fn fc (Cnte b)      = fc b

-------------------------------------------------------------------------------------------------------------------------------

data ExpList a = Filter (a -> Bool) (ExpList a)
               | Map    (a -> a)    (ExpList a)
               | Append (ExpList a) (ExpList a)
               | Reverse (ExpList a)
               | List [a]

foldEL' :: ((a -> Bool) -> b -> b) 
       -> ((a -> a) -> b -> b) 
       -> (b -> b -> b) 
       -> (b -> b) 
       -> ([a] -> b)
       -> ExpList a
       -> b
foldEL' ff fm fa fr fl (Filter fb exp)    = ff fb (foldEL' ff fm fa fr fl exp)
foldEL' ff fm fa fr fl (Map f exp)        = fm f (foldEL' ff fm fa fr fl exp)   
foldEL' ff fm fa fr fl (Append exp1 exp2) = fa (foldEL' ff fm fa fr fl exp1) (foldEL' ff fm fa fr fl exp2)  
foldEL' ff fm fa fr fl (Reverse exp)      = fr (foldEL' ff fm fa fr fl exp)
foldEL' ff fm fa fr fl (List xs)          = fl xs

recEL' :: ((a -> Bool) -> ExpList a -> b -> b) 
       -> ((a -> a) -> ExpList a -> b -> b) 
       -> (ExpList a -> b -> ExpList a -> b -> b) 
       -> (ExpList a -> b -> b) 
       -> ([a] -> b)
       -> ExpList a
       -> b
recEL' ff fm fa fr fl (Filter fb exp)    = ff fb exp (recEL' ff fm fa fr fl exp)
recEL' ff fm fa fr fl (Map f exp)        = fm f exp (recEL' ff fm fa fr fl exp)   
recEL' ff fm fa fr fl (Append exp1 exp2) = fa exp1 (recEL' ff fm fa fr fl exp1) exp2 (recEL' ff fm fa fr fl exp2)  
recEL' ff fm fa fr fl (Reverse exp)      = fr exp (recEL' ff fm fa fr fl exp)
recEL' ff fm fa fr fl (List xs)          = fl xs

--Usando el truquito del go que me enseñó Mati

foldEL :: ((a -> Bool) -> b -> b) 
       -> ((a -> a) -> b -> b) 
       -> (b -> b -> b) 
       -> (b -> b) 
       -> ([a] -> b)
       -> ExpList a
       -> b
foldEL ff fm fa fr fl = go
    where go (Filter fb exp)    = ff fb (go exp)
          go (Map f exp)        = fm f (go exp)   
          go (Append exp1 exp2) = fa (go exp1) (go exp2)  
          go (Reverse exp)      = fr (go exp)
          go (List xs)          = fl xs

recEL :: ((a -> Bool) -> ExpList a -> b -> b) 
       -> ((a -> a) -> ExpList a -> b -> b) 
       -> (ExpList a -> b -> ExpList a -> b -> b) 
       -> (ExpList a -> b -> b) 
       -> ([a] -> b)
       -> ExpList a
       -> b
recEL ff fm fa fr fl = go
    where go (Filter fb exp)    = ff fb exp (go exp)
          go (Map f exp)        = fm f exp (go exp) 
          go (Append exp1 exp2) = fa exp1 (go exp1) exp2 (go exp2)  
          go (Reverse exp)      = fr exp (go exp)
          go (List xs)          = fl xs

--Acá b se instancia en [a]
evalExpList' :: ExpList a -> [a]
evalExpList' = foldEL (\fb xs -> filter fb xs) 
                      (\f xs -> map f xs)
                      (\xs ys -> xs ++ ys)
                      (\xs -> reverse xs)
                      (\xs -> xs)

evalExpList :: ExpList a -> [a]                      
evalExpList = foldEL filter map (++) reverse id

{-
data ExpList a = Filter (a -> Bool) (ExpList a)
               | Map    (a -> a)    (ExpList a)
               | Append (ExpList a) (ExpList a)
               | Reverse (ExpList a)
               | List [a]
-}

{-
foldEL' :: ((a -> Bool) -> b -> b) 
       -> ((a -> a) -> b -> b) 
       -> (b -> b -> b) 
       -> (b -> b) 
       -> ([a] -> b)
       -> ExpList a
       -> b
foldEL' ff fm fa fr fl (Filter fb exp)    = ff fb (foldEL' ff fm fa fr fl exp)
foldEL' ff fm fa fr fl (Map f exp)        = fm f (foldEL' ff fm fa fr fl exp)   
foldEL' ff fm fa fr fl (Append exp1 exp2) = fa (foldEL' ff fm fa fr fl exp1) (foldEL' ff fm fa fr fl exp2)  
foldEL' ff fm fa fr fl (Reverse exp)      = fr (foldEL' ff fm fa fr fl exp)
foldEL' ff fm fa fr fl (List xs)          = fl xs
-}

-- Reglas que permiten simplificar (planteadas por el astuto Fede)
-- (Map f (Map g exs)) = Map (f . g) exs
-- Filter p1 (Filter p2 exs) = Filter (\x -> p1 x && p2 x) exs
-- (Reverse (Reverse exs)) = exs

simpExpList :: ExpList a -> ExpList a
simpExpList (Filter p exp)     = simpFilter p (simpExpList exp)
simpExpList (Map f exp)        = simpMap f (simpExpList exp)
simpExpList (Append exp1 exp2) = Append (simpExpList exp1) (simpExpList exp2)
simpExpList (Reverse exp)      = simpReverse (simpExpList exp)
simpExpList (List xs)          = List xs

simpFilter :: (a -> Bool) -> ExpList a -> ExpList a
simpFilter p1 (Filter p2 exp) = Filter (\x -> p1 x && p2 x) exp --de 2 Filter que tenía, compacté a uno
simpFilter p  exp             = Filter p exp --Reconstruyo el único Filter que tenía

simpMap :: (a -> a) -> ExpList a -> ExpList a
simpMap f1 (Map f2 exp) = Map (f1 . f2) exp
simpMap f  exp          = Map f exp

simpReverse :: ExpList a -> ExpList a
simpReverse (Reverse exp) = exp
simpReverse exp           = Reverse exp

--Acá b está instanciada en ExpList
simpExpList' :: ExpList a -> ExpList a
simpExpList' = foldEL simpFilter simpMap Append simpReverse List

-- Tarea (definir):
-- probablemente requiera rec
-- cuantosReverseConsecutivos :: ExpList a -> Int

-- Tarea (demo):
-- evalExpList . simpExpList = evalExpList



