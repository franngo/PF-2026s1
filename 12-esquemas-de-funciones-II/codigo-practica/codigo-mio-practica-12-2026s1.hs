--Ejercicio 6

data Dir = Izq | Der | Straight
    deriving Show

data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)   
    deriving Show

--6.a.

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









mapa1 = Bifurcacion [3, 4]
            (Nada
                (Cofre [5, 9]))
            (Bifurcacion [7, 12]
                (Bifurcacion [0, 1]
                    (Nada
                        (Cofre [13, 14]))
                    (Cofre [4]))
                (Cofre [21, 22, 23]))

{-
funcion (Cofre xs)               = ... xs
funcion (Nada mp)                = ... (funcion mp)
funcion (Bifurcacion xs mp1 mp2) = ... xs ... (funcion mp1) (funcion mp2)
-}          

{-
data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a)   
-}

--6.b.
objects :: Mapa a -> [a]
objects (Cofre xs)               = xs
objects (Nada mp)                = objects mp
objects (Bifurcacion xs mp1 mp2) = xs ++ (objects mp1) ++ (objects mp2)

mapM' :: (a -> b) -> Mapa a -> Mapa b
mapM' f (Cofre xs)               = Cofre (map f xs)
mapM' f (Nada mp)                = Nada (mapM' f mp)
mapM' f (Bifurcacion xs mp1 mp2) = Bifurcacion (map f xs) (mapM' f mp1) (mapM' f mp2)

has :: (a -> Bool) -> Mapa a -> Bool
has p (Cofre xs)               = any p xs
has p (Nada mp)                = has p mp
has p (Bifurcacion xs mp1 mp2) = any p xs || (has p mp1) || (has p mp2)

--Describe si al final del camino dado en el mapa dado hay algún elemento que cumple el predicado dado.
--Se hace recursión sobre Mapa a y la [Dir] es un parámetro extra.
hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt p (Cofre xs)               ds = if null ds then any p xs else error "el mapa dado era incorrecto"
hasObjectAt p (Nada mp)                ds =
    case ds of
        (Straight:ds') -> hasObjectAt p mp ds'
        _              -> error "el mapa dado era incorrecto"
hasObjectAt p (Bifurcacion xs mp1 mp2) ds = 
    case ds of
        []        -> any p xs
        (Izq:ds') -> hasObjectAt p mp1 ds'
        (Der:ds') -> hasObjectAt p mp2 ds'
        _         -> error "el mapa dado era incorrecto"

--hasObjectAt (==13) mapa1 [Der, Izq, Izq, Straight] = True   

longestPath :: Mapa a -> [Dir]
longestPath (Cofre xs)               = []
longestPath (Nada mp)                = Straight : longestPath mp
longestPath (Bifurcacion xs mp1 mp2) = 
    if length (longestPath mp1) > length (longestPath mp2)
        then Izq : longestPath mp1
        else Der : longestPath mp2

--Se usa TUPLING
--Delegamos a una subtarea donde se calculan dos cosas a la vez, una por cada componente de la tupla, ya que una de estas te sirve para hacer 
--los desempates a la hora de elegir (en este caso, la [Dir])
--Después, en la función principal que usa dicha subtarea, nos quedamos con la componente [a] que es la que nos interesa.
objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath mp = 
    let (objs, dirs) = objsAndLongestPath mp
    in objs

objsAndLongestPath :: Mapa a -> ([a], [Dir])
objsAndLongestPath (Cofre xs)               = (xs, [])
objsAndLongestPath (Nada mp)                = 
    let (objs, dirs) = objsAndLongestPath mp
    in (objs, Straight:dirs)
objsAndLongestPath (Bifurcacion xs mp1 mp2) = 
    let (objs1, dirs1) = objsAndLongestPath mp1
        (objs2, dirs2) = objsAndLongestPath mp2
    in if length dirs1 > length dirs2
        then (xs ++ objs1, Izq:dirs1)
        else (xs ++ objs2, Der:dirs2)    

{-
allPaths :: Mapa a -> [[Dir]], que describe la lista con
todos los caminos del mapa dado.

objectsPerLevel :: Mapa a -> [[a]], que describe la lista
con todos los objetos por niveles del mapa dado.
-}