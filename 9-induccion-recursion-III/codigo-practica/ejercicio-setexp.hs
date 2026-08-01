--Ejercicio dado por Fede en la práctica
--(sin hacer...) (y creo que no lo revisó en clases siguientes)

data SetExp a = EmptyS
              | Singleton a
              | Union (SetExp a) (SetExp a)
              | Intersect (SetExp a) (SetExp a)
              | Diff (SetExp a) (SetExp a) 
    deriving Show     

-- la lista resultante no tiene elementos repetidos
evalSL :: SetExp a -> [a]


evalSF :: SetExp a -> (a -> Bool)
-- vale 

-- por ejemplo:
-- elem e [1,2,3] = (\x -> x == 1 || x == 2 || x == 3) e

-- Propiedad:
-- Para todo e, s. 
-- elem e (evalSL s) = (evalSF s) e
-- Tarea: demostrar la propiedad

-- importante, no evaluar la expresión
simpSE :: SetExp a -> SetExp a
-- Propiedades de simplificación:

-- En la union cuando un conjunto es vacio el resultado
-- es el otro conjunto

-- En la interseccion cuando un conjunto es vacio
-- el resultado es vacio

-- En la diferencia entre conjuntos, restar un conjunto
-- vacio, da el otro conjunto. Y restarle elementos
-- a un conjunto vacio, da vacio.

-- Propiedad:
-- Para s. 
-- evalSF s = evalSF (simpSE s)
-- Tarea: demostrar la propiedad

-- Propiedad:
-- Para s. 
-- evalSL s = evalSL (simpSE s)
-- Tarea: demostrar la propiedad

-- toma los elementos de la lista
-- y una función que dice si van a estar
-- en el conjunto
-- y arma una expresión de conjuntos
-- que representa a dicha función
toSetExp :: [a] -> (a -> Bool) -> SetExp a

-- Demo:
-- Para todo xs, s
-- evalSF (toSetExp xs (evalSF s)) = evalSF s
