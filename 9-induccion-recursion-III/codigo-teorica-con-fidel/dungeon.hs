--Dadas las siguientes definiciones:
data Objeto = Moneda | Pelusa
    deriving Show

data Dungeon = Cueva | Habitacion [Objeto] Dungeon Dungeon
    deriving Show

--objs :: Dungeon -> [Objeto]

doblar :: [Objeto] -> [Objeto]
doblar []     = []
doblar (o:os) = doblarObj o ++ doblar os

doblarObj :: Objeto -> [Objeto]
doblarObj Moneda = [Moneda, Moneda]
doblarObj o      = [o]    

excavar :: Dungeon -> Dungeon
excavar Cueva                 = Cueva
excavar (Habitacion os di dd) = Habitacion (doblar os) (excavar di) (excavar dd)

--1.a. Definir objs
objs :: Dungeon -> [Objeto]
objs Cueva                 = []
objs (Habitacion os di dd) = os ++ objs di ++ objs dd

dung1 = 
    Habitacion [Moneda, Pelusa, Moneda]
        (Habitacion [Moneda, Moneda]
            (Habitacion [Pelusa, Pelusa]
                Cueva Cueva)
            Cueva)
        (Habitacion [Pelusa, Moneda] 
            (Habitacion [Moneda]
                Cueva Cueva)
            (Habitacion []
                Cueva 
                (Habitacion [Pelusa, Pelusa]
                Cueva Cueva)))

{-
2. Dar la función losAntecesoresDe :: a -> Tree a -> [a]
Describe la lista de antecesores del elemento dado dentro del árbol dado, suponiendo que el mismo existe dentro del árbol (o sea, es parcial).
ej: losAntecesoresDe 7 t = [1,3]
-}                

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

t = NodeT 1
        (NodeT 2
            (NodeT 4 EmptyT EmptyT)
            (NodeT 5 EmptyT EmptyT))
        (NodeT 3
            (NodeT 6 EmptyT EmptyT)
            (NodeT 7
                (NodeT 8 EmptyT EmptyT)
                (NodeT 9 EmptyT EmptyT)))

--Se supone que el elemento dado existe dentro del árbol dado
--Esta implementación es costosa, ya que re-escaneo las mismas ramas una y otra vez a medida que bajo mediante el anyT, que es O(N). 
--El costo es cuadrático.
losAntecesoresDe' :: Eq a => a -> Tree a -> [a]
losAntecesoresDe' x EmptyT          = error "El elemento dado no se encuentra en el árbol dado porque es vacío"
losAntecesoresDe' x (NodeT y ti td) = 
    if x == y
        then []
        else let t = elQueContieneElemento x ti td
             in y : (losAntecesoresDe' x t)

elQueContieneElemento :: Eq a => a -> Tree a -> Tree a -> Tree a
elQueContieneElemento x ti td = 
    if anyT (x==) ti
        then ti
        else if anyT (x==) td
            then td
            else error "El elemento dado no se encuentra en ninguno de los subárboles"

anyT :: (a -> Bool) -> Tree a -> Bool
anyT fb EmptyT          = False
anyT fb (NodeT x ti td) = fb x || (anyT fb ti) || (anyT fb td)     

--Versión usando colapso por elección sobre el resultado:
--En vez de preguntar con anyT (O(N)) antes de hacer recursión sobre un subárbol, vamos a hacer la recursión sobre estos, agarrar el
--resultado (un Maybe) y hacer pattern matching y un (:) (O(1)).
--El costo total va a terminar siendo O(N).
losAntecesoresDe :: Eq a => a -> Tree a -> [a]
losAntecesoresDe x t = 
    case losAntecesoresDeSiHay x t of
        Nothing   -> error "El elemento dado no se encuentra en el árbol dado"
        (Just xs) -> xs

losAntecesoresDeSiHay :: Eq a => a -> Tree a -> Maybe [a]
losAntecesoresDeSiHay x EmptyT          = Nothing
losAntecesoresDeSiHay x (NodeT y ti td) =
    if x==y
        then Just []
        else agregarAntecesor y (losAntecesoresDeSiHay x ti) 
                                (losAntecesoresDeSiHay x td)

agregarAntecesor :: a -> Maybe [a] -> Maybe [a] -> Maybe [a]
agregarAntecesor x Nothing   Nothing   = Nothing
agregarAntecesor x (Just xs) Nothing   = Just (x:xs)
agregarAntecesor x Nothing   (Just ys) = Just (x:ys)

        