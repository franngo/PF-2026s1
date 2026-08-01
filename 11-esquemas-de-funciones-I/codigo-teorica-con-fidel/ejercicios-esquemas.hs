--Antes de los ejercicios de la teórica dados por Fidel, primero mi implementación de las funciones a medida que veo el video:

--Esquemas de funciones sobre la estructura de listas:
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) = 
    if p x
        then x : filter' p xs
        else filter' p xs

--Esquema de recursión estructural (sobre listas)
--Es el único esquema que tiene el lambda recursivo antes que el valor base/lambda base
foldr' :: (a -> b -> b) -> b -> [a] -> b        
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

--ej: foldr' (+) 0 [4,2,3,5] = 14

sumarTodos = foldr' (+) 0

--sumarTodos [4,2,3,5] = 14

sonCincos = foldr ((&&) . (==5)) True

-------------------------------
--CASOS DE PARÁMETRO EXTRA:

--1: Es parte del resultado y no cambia (parámetro estático) 

--forma 1 (avalada): se pasa al resultado del foldr, en este caso, a \ys -> ...
--Acá el parámetro recursivo es la 1ra [a]!! (importante)
--Aca b equivale a [a] -> [a]
append :: [a] -> [a] -> [a]
append = foldr (\x h -> \ys -> x : h ys) (\ys -> ys)
                                          --o id

--forma 2 (avalada): Entiendo que, para este caso donde el parámetro extra está antes que el parámetro recursivo, 
--también es válido usar el parámetro extra como global (como en este ejemplo de Fidel).
--Supongo que siempre que el parámetro sea estático y vaya antes que el param recursivo, se puede usar como global.
--Acá b se instancia en [a]
insertt :: Ord a => a -> [a] -> [a]
insertt x = recr [x] (\y ys rs -> if x < y 
                                    then x : y : ys 
                                    else y : rs)                                          

--2: No es parte del resultado, pero no cambia (parámetro estático) 

--forma 1 (avalada): Versión con el a (parámetro extra) como parámetro global
--En este caso, la función que se define es "elem y" y no "elem"
--Acá b equivale a Bool
elem'' :: Eq a => a -> [a] -> Bool
elem'' y = foldr (\x b -> x==y || b) False

--forma 2 (avalada): se pasa al resultado del foldr, en este caso, a \y -> ...
--Acá b equivale a a -> Bool
elem' :: Eq a => a -> [a] -> Bool
elem' = flip (foldr (\x h -> \y -> x==y || h y) (const False))

--(se pasa al resultado del foldr, en este caso, a \fb -> ...)
--Acá b equivale a (a -> Bool) -> Bool
allTrue :: [a] -> (a -> Bool) -> Bool
allTrue = foldr (\x b -> \fb -> fb x && b fb) (const True)

--se pasa al resultado del foldr, en este caso, a \ys -> ... / const True
--Acá el parámetro recursivo es la 1ra [a]!! (importante)
--Acá b equivale a [a] -> Bool
subset :: Eq a => [a] -> [a] -> Bool
subset = foldr (\x h -> \ys -> elem x ys && h ys) (const True)

--3. No es parte del resultado, pero sí cambia con la recursión (parámetro dinámico) 

--Acá hay que tener en cuenta que estamos haciendo recursión SOBRE LA ESTRUCTURA DE LISTA y no sobre el Int
take' :: Int -> [a] -> [a]
take' n []     = []
take' n (x:xs) = 
    if n > 0
        then x : take' (n-1) xs 
        else []

--única forma: NO se usa como parámetro global, si no que se pasa al resultado del foldr
--Notar que a la recursión (h) se le pasa el parámetro cambiado.
--Como el parámetro extra está antes del parámetro recursivo, debo usar flip.
--Aca b se instancia en Int -> [a]
take'' = flip (foldr (\x h -> \n -> if n > 0 then x : h (n-1) else []) (const []))

--Otra forma válida es usando un where para el lambda recursivo, ya que es medio largo.
--Acá si se nota bien que se le pasa el parámetro extra al resultado del foldr.
take''' = flip (foldr cr (const []))
    where cr x h = \n -> if n > 0 then x : h (n-1) else []

--Otros ejemplos de foldeo:

cantTotal :: [[a]] -> Int
cantTotal []       = 0
cantTotal (xs:xss) = length xs + cantTotal xss

cantTotal' = foldr ((+) . length) 0

--versión más eficiente usando estrategia map-reduce:
--para todo f. g. z.
--foldr (f . g) z = foldr f z . map g
cantTotal'' = foldr (+) 0 . map length

-------------------------------
--Esquema de recursión primitiva (sobre listas)
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b    
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

maximum' :: Ord a => [a] -> a
maximum' []     = error "No hay un máximo en una lista vacía"
maximum' (x:xs) = 
    case xs of
        []  -> x
        xs' -> max x (maximum xs')

--Quizás sería mejor usar la variable m en vez de y
maximum'' :: Ord a => [a] -> a
maximum'' = recr (error "No hay un máximo en una lista vacía") (\x xs y -> case xs of
                                                                            []  -> x
                                                                            xs' -> max x y)        

maximum''' :: Ord a => [a] -> a
maximum''' = recr (error "No hay un máximo en una lista vacía") cr
    where cr x xs y = case xs of
                        []  -> x
                        xs' -> max x y

insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (y:ys) = 
    if x < y
        then x : y : ys
        else y : insert x ys

--b en este caso se instancia en a -> [a]
insert' :: Ord a => a -> [a] -> [a]
insert' = flip (recr (\x -> [x]) (\y ys h -> \x -> if x < y 
                                                    then x : y : ys 
                                                    else y : h x))

--Otra opción (que es la que hizo Fidel) es usar el a como parámetro global.    
--Acá b se instancia en [a]
insert'' :: Ord a => a -> [a] -> [a]
insert'' x = recr [x] (\y ys rs -> if x < y 
                                    then x : y : ys 
                                    else y : rs)

--Describe la lista dada sin el último elemento
init' :: [a] -> [a]
init' []     = error "No se puede eliminar un elemento de una lista vacía"
init' (x:xs) = 
    if null xs
        then []
        else x : init' xs

init'' :: [a] -> [a]
init'' = recr cb cr
    where cb         = error "No se puede eliminar un elemento de una lista vacía"
          cr x xs rs = if null xs
                        then []
                        else x : rs


--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------

--Ejercicios dados en la clase teórica 11 por Fidel:

sumSqr :: [Int] -> Int
sumSqr = foldr ((+) . (^2)) 0

--Podemos encararlo usando la estrategia map-reduce
sumSqr' :: [Int] -> Int
sumSqr' = (foldr (+) 0) . (map (^2))
 
--Acá b equivale a [a] -> Bool
subset' :: Eq a => [a] -> [a] -> Bool
subset' = foldr (\x h -> \ys -> elem x ys && h ys) (const True)

--Horrorosa versión en cuanto al costo algorítmico (que sería cuadrático)
accumSum' []     = []
accumSum' (n:ns) = (n + sum ns) : accumSum' ns 

--Una versión mucho mejor en cuanto a costo algorítmico
accumSum []     = []
accumSum (n:ns) = case accumSum ns of
                        []      -> [n]
                        (n':ns') -> n + n' : n' : ns'

--Acá b se instancia en [Int]
accumSum'' :: [Int] -> [Int]
accumSum'' = foldr cr []
    where cr n rs = case rs of
                        []       -> [n]
                        (n':ns') -> n + n' : n' : ns'       

