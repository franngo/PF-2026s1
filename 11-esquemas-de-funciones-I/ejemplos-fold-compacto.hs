data Calabozo g r = Salida | Celda r | Pasillo Int (Calabozo g r)| Bifurcacion (Calabozo g r) g (Calabozo g r)

foldCalabozo :: b -> (r -> b) -> (Int -> b -> b) -> (b -> g -> b -> b) -> Calabozo g r -> b
foldCalabozo fs fc fp fb = go
          where go Salida                  = fs
                go (Celda r)               = fc r
                go (Pasillo n cl)          = fp n (go cl)
                go (Bifurcacion cl1 g cl2) = fb (go cl1) g (go cl2)

data Pizza = Prepizza | Capa Ingrediente Pizza

foldPizza :: b -> (Ingrediente -> b -> b) -> Pizza -> b
foldPizza z fc = go
         where go Prepizza   = z
               go (Capa i p) = fc i (go p)
               
data ExpA = Cte n | Sum ExpA ExpA | Prod ExpA ExpA

foldExpA :: (n -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExpA -> b
foldExpA fc fs fp = go 
        where go (Cte n)      = fc n
              go (Sum e1 e2)  = fs (go e1) (go e2)
              go (Prod e1 e2) = fp (go e1) (go e2)               
