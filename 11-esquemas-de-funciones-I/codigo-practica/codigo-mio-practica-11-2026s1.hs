--Ejercicio 7:

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

--Esquema de recursión primitiva (sobre listas)
recr :: b -> (a -> [a] -> b -> b) -> [a] -> b    
recr z f []     = z
recr z f (x:xs) = f x xs (recr z f xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _      _      = []

--Este esquema solo trabaja con listas con al menos un elemento y no con listas vacías
--Debido a esto, no necesita que le pasemos un valor base explícito, sino que toma el último elemento de la lista como su valor base.
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f [] = error "No se puede usar este tipo de foldr con listas vacías"
foldr1' f (x:xs) = 
    case xs of
        [] -> x 
        xs'-> f x (foldr1' f xs')

--scanr (+) 0 [1, 2, 3] = [6, 5, 3, 0]
--Si foldr toma una lista y la "colapsa" de derecha a izquierda en un único valor final, scanr hace casi lo mismo pero con una 
--diferencia clave: va guardando todos los resultados intermedios (acumulados) en una lista, desde el final hasta el principio.
--La lista resultante va a empezar con el resultado final acumulado a la izquierda, y va a terminar con el valor base a la derecha.
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f z []     = [z]
scanr' f z (x:xs) = 
    let (r:rs) = scanr' f z xs   
    in f x r : r : rs

--Ejercicio 9:

sum' :: [Int] -> Int
sum' = foldr (+) 0

length' :: [a] -> Int
length' = foldr (const (1+)) 0

--En este caso, b se instancia en [b]
map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr (\x rs -> f x : rs) []

--En este caso, b se instancia en [a]
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr cr []
    where cr x rs  = if p x
                        then x : rs
                        else rs

--En este caso, b se instancia en Maybe a
find :: (a -> Bool) -> [a] -> Maybe a    
find p = foldr cr Nothing
    where cr x m = if p x
                    then Just x
                    else m

--En este caso, b se instancia en Bool
any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr ((||) . p) False

--En este caso, b se instancia en Bool
all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr ((&&) . p) True

--En este caso, b se instancia en Int
countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldr ((+) . unoSiCeroSino p) 0

unoSiCeroSino :: (a -> Bool) -> a -> Int
unoSiCeroSino p x =
    if p x
        then 1
        else 0

--Primero la pienso con rec. estructural explícita
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p []     = ([],[])
partition p (x:xs) = agregarDondeCorresponde p x (partition p xs)

agregarDondeCorresponde :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
agregarDondeCorresponde p x (sis, nos) =
    if p x
        then (x:sis, nos)
        else (sis, x:nos)

--En este caso, b se instancia en ([a], [a])
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p = foldr (agregarDondeCorresponde p) ([],[]) 

--Este no lo pedía en la práctica, pero Fidel me recomendó hacerlo
zip1 :: [a] -> [b] -> [(a,b)]
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys
zip1 _      _      = []

--Este es un caso de parámetro extra que forma parte del resultado y que SÍ cambia
--Acá la b de foldr se instancia en [b'] -> [(a',b')]
{-
ROMPE PARA EL CASO EN QUE LA PRIMERA LISTA ES MAYOR. Esto es porque el foldr está haciendo la recursión sobre la
primera lista, y la segunda es solo un parámetro extra que se pasa al resultado del foldr. 
Por tanto, siempre va a matchear con el caso base o recursivo de la definición de foldr en base a la primera lista. 
En caso de aún tener elementos en la 1ra pero no en la 2da, matchea con el caso recursivo igualmente y termina 
rompiendo por cómo es el lambda del caso recursivo.
Habría que repensar el lambda recursivo.
-}
zip1' :: [a] -> [b] -> [(a,b)]
zip1' = foldr (\x h -> \ys -> (x, head ys) : h (tail ys)) (const [])

--Ahora la pienso simplemente en términos de caso base y caso recursivo para la estructura sobre la que hago recursión.
zip2 :: [a] -> [b] -> [(a,b)]
zip2 []     _  = []
zip2 (x:xs) ys = 
    case ys of
        []      -> []
        (y:ys') -> (x,y) : zip2 xs ys'

--Acá la b de foldr se instancia en [b'] -> [(a',b')]
zip2' :: [a] -> [b] -> [(a,b)]
zip2' = foldr cr (const [])
    where cr x h = \ys -> case ys of
                            []      -> []
                            (y:ys') -> (x,y) : h ys'


{-
j. zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
k. scanr :: (a -> b -> b) -> b -> [a] -> [b]
l. takeWhile :: (a -> Bool) -> [a] -> [a]
m. take :: Int -> [a] -> [a]
n. drop :: Int -> [a] -> [a]
o. elemAt :: Int -> [a] -> a
-}





