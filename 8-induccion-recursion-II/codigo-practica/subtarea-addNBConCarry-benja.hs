--Sección II. Ejercicio 3.a.
--Subtarea sugerida en el pdf de la práctica para addNB hecha por Benja

addNBConCarry :: NBin -> NBin -> DigBin -> NBin
addNBConCarry []       []       O = []
addNBConCarry []       []       I = [I]
addNBConCarry []       (mb:mbs) c = let (s', c') = addDBConCarry O mb c
                                        in s' : addNBConCarry [] mbs c'
addNBConCarry (nb:nbs) []       c = let (s', c') = addDBConCarry nb O c
                                        in s' : addNBConCarry nbs [] c'
addNBConCarry (nb:nbs) (mb:mbs) c = let (s', c') = addDBConCarry nb mb c
                                        in s' : addNBConCarry nbs mbs c'

addDBConCarry :: DigBin -> DigBin -> DigBin -> (DigBin, DigBin)
addDBConCarry I I O = (O, I)
addDBConCarry I I I = (I, I)
addDBConCarry O O c = (c, O)
addDBConCarry _ _ I = (O, I)
addDBConCarry _ _ O = (I, O)
