--Ejercicio 1.a.

data EA = Const Int | BOp BinOp EA EA
    deriving Show

data BinOp = Sum | Mul
    deriving Show

data ExpA = Cte Int | Suma ExpA ExpA | Prod ExpA ExpA
    deriving Show


{-
funcion (Const n)       = ...
funcion (BOp bop e1 e2) = bop ... (funcion e1) ... (funcion e2)
-}    

cte0 = Const 0
cte1 = Const 1
cte2 = Const 2
cte3 = Const 3
suma01 = BOp Sum cte0 cte1
suma21 = BOp Sum cte2 cte1
suma32 = BOp Sum cte3 cte2
mul01 = BOp Mul cte0 cte1
mul21 = BOp Mul cte2 cte1
mul32 = BOp Mul cte3 cte2
suma63 = BOp Sum mul32 suma21

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

---------------------
evalEA :: EA -> Int
evalEA (Const n)       = n
evalEA (BOp bop e1 e2) = case bop of
                            Sum -> (evalEA e1) + (evalEA e2)
                            Mul -> (evalEA e1) * (evalEA e2)

---------------------
ea2ExpA :: EA -> ExpA
ea2ExpA (Const n)       = Cte n
ea2ExpA (BOp bop e1 e2) = case bop of
                            Sum -> Suma (ea2ExpA e1) (ea2ExpA e2)
                            Mul -> Prod (ea2ExpA e1) (ea2ExpA e2)  

---------------------
expA2ea :: ExpA -> EA
expA2ea (Cte n)          = Const n
expA2ea (Suma exp1 exp2) = BOp Sum (expA2ea exp1) (expA2ea exp2)
expA2ea (Prod exp1 exp2) = BOp Mul (expA2ea exp1) (expA2ea exp2)                                           

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

--Ejercicio 2.a.

data Arbol a b = Hoja b | Nodo a (Arbol a b) (Arbol a b)
    deriving Show

{-
funcion (Hoja x)       = ...
funcion (Nodo x ti td) = x ... (funcion ti) ... (funcion td)
-}    

---------------------
cantidadDeHojas :: Arbol a b -> Int
cantidadDeHojas (Hoja x)       = 1
cantidadDeHojas (Nodo x ti td) = (cantidadDeHojas ti) + (cantidadDeHojas td)

---------------------
cantidadDeNodos :: Arbol a b -> Int
cantidadDeNodos (Hoja x)       = 0
cantidadDeNodos (Nodo x ti td) = 1 + (cantidadDeNodos ti) + (cantidadDeNodos td)

---------------------
cantidadDeConstructores :: Arbol a b -> Int
cantidadDeConstructores (Hoja x)       = 1
cantidadDeConstructores (Nodo x ti td) = 1 + (cantidadDeConstructores ti) + (cantidadDeConstructores td)

---------------------
ea2Arbol :: EA -> Arbol BinOp Int
ea2Arbol (Const n)       = Hoja n
ea2Arbol (BOp bop e1 e2) = Nodo bop (ea2Arbol e1) (ea2Arbol e2)

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

--Ejercicio 3.a.

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

{-
funcion EmptyT          = ...
funcion (NodeT x ti td) = x ... (funcion ti) ... (funcion td)    
-}   

em = EmptyT

nodo9 = NodeT 9 em em
nodo8 = NodeT 8 nodo9 em
nodo7 = NodeT 7 nodo8 em
nodo6 = NodeT 6 em em
nodo5 = NodeT 5 em em
nodo4 = NodeT 4 em em
nodo3 = NodeT 3 nodo6 nodo7
nodo2 = NodeT 2 nodo4 nodo5
nodo1 = NodeT 1 nodo2 nodo3

node8 = NodeT 8 em em
node7 = NodeT 7 node8 em
node6 = NodeT 6 em em
node5 = NodeT 5 em em
node4 = NodeT 4 em em
node3 = NodeT 3 node6 node7
node2 = NodeT 2 node4 node5
node1 = NodeT 1 node2 node3

---------------------
sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n ti td) = n + (sumarT ti) + (sumarT td)

---------------------
sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT _ ti td) = 1 + (sizeT ti) + (sizeT td)  

---------------------
anyT :: (a -> Bool) -> Tree a -> Bool
anyT fb EmptyT          = False
anyT fb (NodeT x ti td) = fb x || (anyT fb ti) || (anyT fb td)    

---------------------
countT :: (a -> Bool) -> Tree a -> Int
countT fb EmptyT          = 0
countT fb (NodeT x ti td) = unoSi fb x + (countT fb ti) + (countT fb td)    

unoSi:: (a -> Bool) -> a -> Int
unoSi fb x = if fb x
                then 1
                else 0

---------------------
countLeaves :: Tree a -> Int
countLeaves EmptyT          = 0
countLeaves (NodeT _ ti td) = unoSiHoja ti td + (countLeaves ti) + (countLeaves td)    

unoSiHoja:: Tree a -> Tree a -> Int
unoSiHoja EmptyT EmptyT = 1
unoSiHoja _      _      = 0

---------------------
heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT _ ti td) = 1 + max (heightT ti) (heightT td)  

---------------------
inOrder :: Tree a -> [a]
inOrder EmptyT          = []
inOrder (NodeT x ti td) = agregarInOrder x (inOrder ti) (inOrder td)  

agregarInOrder:: a -> [a] -> [a] -> [a]
agregarInOrder x xs ys = xs ++ [x] ++ ys

--La subtarea no era realmente necesaria. Podría haber hecho directamente así:
inOrder' :: Tree a -> [a]
inOrder' EmptyT          = []
inOrder' (NodeT x ti td) = inOrder' ti ++ [x] ++ inOrder' td

---------------------
levelN :: Int -> Tree a -> [a]
levelN n EmptyT          = []
levelN n (NodeT x ti td) = if n==0
                            then [x]
                            else (levelN (n-1) ti) ++ (levelN (n-1) td)

---------------------
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x ti td) = agregarAMasLarga x (ramaMasLarga ti) (ramaMasLarga td)   

agregarAMasLarga:: a -> [a] -> [a] -> [a]
agregarAMasLarga x xs ys = if length xs > length ys
                            then x : xs
                            else x : ys

--Nuevamente, la subtarea no era realmente necesaria. Podría haber hecho directamente así:
ramaMasLarga' :: Tree a -> [a]
ramaMasLarga' EmptyT          = []
ramaMasLarga' (NodeT x ti td) = if length (ramaMasLarga' ti) > length (ramaMasLarga' td)   
                                    then x : ramaMasLarga' ti
                                    else x : ramaMasLarga' td

--Otra def con let (pero que justo no nos sirve para la demo):
ramaMasLarga'' :: Tree a -> [a]
ramaMasLarga'' EmptyT          = []
ramaMasLarga'' (NodeT x ti td) = let ri = ramaMasLarga'' ti
                                     rd = ramaMasLarga'' td
                                     in if length ri > length rd   
                                            then x : ri
                                            else x : rd     

---------------------                          
--En esta versión, tenemos en cuenta también los caminos intermedios y no solo los caminos hasta las hojas.            
todosLosCaminosConIntermedios :: Tree a -> [[a]]
todosLosCaminosConIntermedios EmptyT          = []
todosLosCaminosConIntermedios (NodeT x ti td) = 
    [x] : map (x:) (todosLosCaminosConIntermedios ti) ++ 
          map (x:) (todosLosCaminosConIntermedios td)  

--Costo O(n * 2^(h-1)), siendo n la cantidad de nodos no hoja del tree y siendo h la altura del tree (y 2^(h-1) es la cantidad de hojas y caminos)
--En esta versión, solo describimos los caminos hacia las hojas.
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x ti td) = 
    case (ti, td) of
        (EmptyT, EmptyT) -> [[x]]
        (ti, td)         -> map (x:) (todosLosCaminos ti) ++ 
                            map (x:) (todosLosCaminos td)    

---------------------  
--Es parcial y recursión primitiva
maxT :: Ord a => Tree a -> a
maxT EmptyT          = error "No hay un máximo en un tree vacío"
maxT (NodeT x ti td) = case (ti, td) of
                        (EmptyT, EmptyT) -> x
                        (ti,     EmptyT) -> max x (maxT ti) 
                        (EmptyT, td    ) -> max x (maxT td)
                        (ti    , td    ) -> maxEntreTres x (maxT ti) (maxT td)  

maxEntreTres:: Ord a => a -> a -> a -> a
maxEntreTres x y z = if x > y
                        then max x z
                        else max y z

--Versión sin la subtarea
maxT' :: Ord a => Tree a -> a
maxT' EmptyT          = error "No hay un máximo en un tree vacío"
maxT' (NodeT x ti td) = case (ti, td) of
                        (EmptyT, EmptyT) -> x
                        (ti,     EmptyT) -> max x (maxT' ti) 
                        (EmptyT, td    ) -> max x (maxT' td)
                        (ti    , td    ) -> x `max` (maxT' ti) `max` (maxT' td)                          

--En vez de implementarla como parcial, lo que puedo hacer es que devuelva Maybe a, y, usando el Nothing, hacer una especie de "handling".
--Lo bueno de esta es que no revisamos ti y td como en la anterior (ya no más recursión primitiva)
maxTM :: Ord a => Tree a -> Maybe a
maxTM EmptyT          = Nothing
maxTM (NodeT x ti td) = (Just x) `maxMaybe` (maxTM ti) `maxMaybe` (maxTM td)  

maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing  Nothing  = Nothing
maxMaybe (Just x) (Just y) = Just (max x y)
maxMaybe Nothing  m2       = m2
maxMaybe m1       Nothing  = m1

---------------------  
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x ti td) = 
    [x] : juntarListasDeNiveles (listPerLevel ti) (listPerLevel td)

juntarListasDeNiveles :: [[a]] -> [[a]] -> [[a]]
juntarListasDeNiveles []       yss      = yss
juntarListasDeNiveles xss      []       = xss
juntarListasDeNiveles (xs:xss) (ys:yss) = 
    (xs ++ ys) : juntarListasDeNiveles xss yss

---------------------  
mirrorT :: Tree a -> Tree a
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x ti td) = NodeT x (mirrorT td) (mirrorT ti)      


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
    
             