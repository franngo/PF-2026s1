--Sección I

data Pizza = Prepizza | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Queso | Jamon | Aceitunas Int | Anchoas | Cebolla | Salsa
    deriving Show

{-
Reglas para la definición del conjunto inductivo Pizza:

regla base 1. Prepizza pertenece al conjunto
regla inductiva 1. Si existe un p::Pizza y un i::Ingrediente, entonces Capa i p pertenece al conjunto

Esquema recursivo sobre la estructura de Pizza:
func Prepizza   = ...
func (Capa i p) = ... i ... func p 
-}    

{-
instancias:
(Capa Jamon (Capa (Aceitunas 8) (Capa (Aceitunas 7) Prepizza)))
(Capa Jamon (Capa Queso (Capa (Aceitunas 7) Prepizza)))
-}

--Ejercicio 3:

--a
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

--b
cantidadDeAceitunas​ Prepizza   = 0
cantidadDeAceitunas​ (Capa i p) = cantAceitunas i + cantidadDeAceitunas​ p

cantAceitunas (Aceitunas n) = n
cantAceitunas _             = 0

--c
duplicarAceitunas​ Prepizza   = Prepizza
duplicarAceitunas​ (Capa i p) = dupAceitunas i (duplicarAceitunas​ p)

dupAceitunas (Aceitunas n) p = Capa (Aceitunas (n*2)) p
dupAceitunas i             p = Capa i p

--opción 2:

duplicarAceitunas2​ Prepizza   = Prepizza
duplicarAceitunas2​ (Capa i p) = Capa (dupAceitunas2 i) (duplicarAceitunas2​ p)

dupAceitunas2 (Aceitunas n) = Aceitunas (n*2)
dupAceitunas2 i             = i

--d
sinLactosa​ Prepizza   = Prepizza
sinLactosa​ (Capa i p) = sacarLactosa i (sinLactosa​ p)

sacarLactosa Queso p = p
sacarLactosa i     p = Capa i p

--e
aptaIntolerantesLactosa​ Prepizza   = True
aptaIntolerantesLactosa​ (Capa i p) = not (esQueso i) && aptaIntolerantesLactosa​ p 

esQueso Queso = True
esQueso _     = False

--f
conDescripcionMejorada​ Prepizza   = Prepizza
conDescripcionMejorada​ (Capa i p) = juntarAceitunas i (conDescripcionMejorada​ p)

juntarAceitunas (Aceitunas n) (Capa (Aceitunas m) p) = Capa (Aceitunas (n+m)) p
juntarAceitunas i              p                     = Capa i p

--otro

juntarPizzas Prepizza   p   = p
juntarPizzas (Capa i p1) p2 = Capa i (juntarPizzas p1 p2)

--Sección II

type Nombre = String

data Planilla = Fin | Registro Nombre Planilla
    deriving Show

data Equipo = Becario Nombre | Investigador Nombre Equipo Equipo Equipo
    deriving Show

{-
Reglas para la definición del conjunto inductivo Planilla:

regla base 1. Fin pertenece al conjunto
regla inductiva 1. Si existe un p::Planilla y un n::Nombre, entonces Registro n p pertenece al conjunto

Esquema recursivo sobre la estructura de Planilla:
func Fin            = ...
func (Registro n p) = ... n ... func p 

Reglas para la definición del conjunto inductivo Equipo:

regla base 1. Si existe un n::Nombre, entonces Becario n pertenece al conjunto
regla inductiva 1. Si existe un n::Nombre, un e1::Equipo, un e2::Equipo y un e3::Equipo, entonces Investigador n e1 e2 e3 pertenece al conjunto

Esquema recursivo sobre la estructura de Equipo:
func (Becario n)               = ... n
func (Investigador n e1 e2 e3) = ... n ... func e1 ... func e2 ... func e3
-}   

--Ejercicio 3:

--a
largoDePlanilla​ Fin            = 0
largoDePlanilla​ (Registro _ p) = 1 +  largoDePlanilla​ p 

--b
esta​ n Fin             = False
esta​ n (Registro n' p) = n==n' || esta​ n p 

--c
juntarPlanillas​ Fin            p' = p'
juntarPlanillas​ (Registro n p) p' = Registro n (juntarPlanillas​ p p')

--d
nivelesJerarquicos​ (Becario _              ) = 1
nivelesJerarquicos​ (Investigador _ e1 e2 e3) = 1 + mayorEntre (nivelesJerarquicos​ e1) (nivelesJerarquicos​ e2) (nivelesJerarquicos​ e3)

mayorEntre n1 n2 n3 = if n1 > n2
                        then max n1 n3
                        else max n2 n3

--e                        
cantidadDeIntegrantes​ (Becario n)               = 1
cantidadDeIntegrantes​ (Investigador n e1 e2 e3) = 1 + cantidadDeIntegrantes​ e1 + cantidadDeIntegrantes​ e2 + cantidadDeIntegrantes​ e3

--f
planillaDeIntegrantes​ (Becario n)               = Registro n Fin
planillaDeIntegrantes​ (Investigador n e1 e2 e3) = Registro n 
                                                (juntarPlanillas​ (planillaDeIntegrantes​ e1) 
                                                (juntarPlanillas​ (planillaDeIntegrantes​ e2) (planillaDeIntegrantes​ e3) ) )

--Sección III

data Dungeon a = Habitacion a | Pasaje (Maybe a) (Dungeon a) | Bifurcacion (Maybe a) (Dungeon a) (Dungeon a)
    deriving Show

{-
Reglas para la definición del conjunto inductivo Dungeon A: (A es una metavariable, ya que esto no es una definición de Haskell)

regla base 1. Si existe un x::a, entonces Habitacion x pertenece al conjunto
regla inductiva 1. Si existe un m::Maybe a y un d::Dungeon, entonces Pasaje m d pertenece al conjunto
regla inductiva 2. Si existe un m::Maybe a, un d1::Dungeon y un d2::Dungeon, entonces Bifurcacion m d1 d1 pertenece al conjunto.

Esquema recursivo sobre la estructura de Dungeon:
func (Habitacion x)        = ... x
func (Pasaje m d)          = ... m ... func d
func (Bifurcacion m d1 d2) = ... m ... func d1 ... func d2
-}   

--Ejercicio 3:             

cantidadDeBifurcaciones​ (Habitacion x)        = 0
cantidadDeBifurcaciones​ (Pasaje m d)          = 0 + cantidadDeBifurcaciones​ d
cantidadDeBifurcaciones​ (Bifurcacion m d1 d2) = 1 + cantidadDeBifurcaciones​ d1 + cantidadDeBifurcaciones​ d2

cantidadDePuntosInteresantes​ (Habitacion x)        = 0
cantidadDePuntosInteresantes​ (Pasaje m d)          = 1 + cantidadDePuntosInteresantes​ d
cantidadDePuntosInteresantes​ (Bifurcacion m d1 d2) = 1 + cantidadDePuntosInteresantes​ d1 + cantidadDePuntosInteresantes​ d2

cantidadDePuntosVacios​ (Habitacion x)        = 0
cantidadDePuntosVacios​ (Pasaje m d)          = unoSiVacio m + cantidadDePuntosVacios​ d
cantidadDePuntosVacios​ (Bifurcacion m d1 d2) = unoSiVacio m + cantidadDePuntosVacios​ d1 + cantidadDePuntosVacios​ d2

unoSiVacio Nothing = 1
unoSiVacio _       = 0

cantidadDePuntosCon​ :: Eq a => a -> Dungeon a -> Integer
cantidadDePuntosCon​ x (Habitacion x')       = 0
cantidadDePuntosCon​ x (Pasaje m d)          = unoSiTiene x m + cantidadDePuntosCon​ x d
cantidadDePuntosCon​ x (Bifurcacion m d1 d2) = unoSiTiene x m + cantidadDePuntosCon​ x d1 + cantidadDePuntosCon​ x d2

unoSiTiene x (Just x') = if x == x'
                            then 1
                            else 0
unoSiTiene _ _         = 0        

esLineal​ (Habitacion x)        = True
esLineal​ (Pasaje m d)          = True && esLineal​ d --incluso esLineal d, porque True es neutro de &&
esLineal​ (Bifurcacion m d1 d2) = False --acá el esquema de recursión sobre Dungeon a está implícito

llenoDe​ x (Habitacion x')       = x == x'
llenoDe​ x (Pasaje m d)          = tieneA x m && llenoDe x​ d
llenoDe​ x (Bifurcacion m d1 d2) = tieneA x m && llenoDe​ x d1 && llenoDe​ x d2

tieneA x (Just x') = x == x'
tieneA _ _         = False     

data Tesoro = Cofre | Oro | Joyas
    deriving (Show, Eq)

dungeoncita = (Bifurcacion (Just Cofre) (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Pasaje (Just Oro) (Habitacion Cofre))) (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro)) (Pasaje Nothing (Habitacion Joyas))))
dungPasaje = (Pasaje Nothing (Pasaje Nothing (Habitacion 2)))
dungPasaje2 = (Pasaje Nothing (Pasaje Nothing (Habitacion Joyas)))
dungPasaje3 = (Pasaje (Just Joyas) (Pasaje (Just Joyas) (Habitacion Joyas)))

{-
instancias:

Bifurcacion (Just Cofre) 
(Bifurcacion Nothing 
(Pasaje Nothing (Habitacion Joyas)) 
(Pasaje (Just Oro) (Habitacion Cofre))) 
(Bifurcacion Nothing 
(Pasaje (Just Oro) (Habitacion Oro)) 
(Pasaje Nothing (Habitacion Joyas)))

la misma pero para usar en el intérprete:
(Bifurcacion (Just Cofre) (Bifurcacion Nothing (Pasaje Nothing (Habitacion Joyas)) (Pasaje (Just Oro) (Habitacion Cofre))) (Bifurcacion Nothing (Pasaje (Just Oro) (Habitacion Oro)) (Pasaje Nothing (Habitacion Joyas))))
-}
