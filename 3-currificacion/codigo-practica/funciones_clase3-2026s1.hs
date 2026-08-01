--funciones anteriores (clase 1 y 2) pero ahora currificadas

--a -> a
--id x = x

--succ x = 1 + x

doble x = x + x

cuadruple x = 4 * x

--(a -> a) -> a -> a
twice f x = f (f x)    

twice' f = g
    where g x = f (f x)

elDobleLuegoDe f = (\x -> (f x) * 2)

--(a -> b) -> a -> b
apply f x = f x

--Int -> Int -> Int
suma x y = x + y

--(Int, Int) -> Int
suma' (x,y) = x + y

--a -> b -> a
--const x y = x    

--(b -> a -> c) -> a -> b -> c
--flip f x y = f y x

--(a -> b -> c) -> (a -> b) -> a -> c
subst f g x = f x (g x)               

 --(b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)            

--(a,b) -> a (versión no currificada de const)
first (x, y) = x

swap (x, y) = (y, x)    

--versión currificada de swap
--a -> b -> (b,a)
riap x y = (y,x)

--versión no currificada que tiene un par como argumento y usa swap
--((b,a) -> c) -> (a,b) -> c
--uflip f p = f (swap p)    

--versión currificada que usa riap
--((b,a) -> c) -> a -> b -> c
uflip f x y = f (riap x y)

--((a,a) -> b) -> a -> b
appDup f x = f (x,x)    

--(a -> b) -> (a -> c) -> a -> (b,c)
appFork f g x = (f x, g x)      

--(a -> c) -> (b -> d) -> a -> b -> (c,d)
appPar f g x y = (f x, g y)    

--(a -> b) -> a -> a -> (b,b)
appDist f x y = (f x, f y)    

-------------------------------------------------------------------------------

--funciones clase 3

--((a,b) -> c) -> a -> b -> c
curry f = \x -> \y -> f (x,y)

--(a -> b -> c) -> (a,b) -> c
uncurry f = \px -> let (x,y) = px in f x y

many 0 _ x = x
many n f x = f (many (n-1) f x)

--Versiones de many para ejercicio 7 de práctica 6, donde se nos pide definir many usando (.) y la menor cantidad de parámetros posibles
--Int -> (a -> a) -> a -> a
many' 0 _ = id
many' n f = compose f (many (n-1) f)

--ej: many' 2 doble = compose doble (compose doble id) = 
--y si le pasamos un argumento
--many' 2 doble 2 = (compose doble (compose doble id)) 2 = doble (compose doble id 2) = doble (doble (id 2)) = doble 4 = 8

--esta versión no funciona como many :(
--many2 0 = flip const
--many2 n = subst . many (n-1)

many3 0 = flip const
many3 n = \f -> f . many (n-1) f

many4 0 = flip const . id
many4 n = \f -> f . many (n-1) f

many5 0 = flip const . id
many5 n = subst (.) (many5 (n-1))
--yo quiero tener una función que tome f y describa f . g f
--La función que me sirve para esto es subst f g x = f x (g x)

--ejercicio 9

cuadruple' x = compose doble doble x

timesTwoPlusThree x = flip suma 3 (doble x)

fourTimes f x = twice twice f x
