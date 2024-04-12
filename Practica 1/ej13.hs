data AB a = Nil | Bin (AB a) a (AB a)

{- Usando recursión explícita, definir los esquemas de recursión estructural (foldAB) y primitiva (recAB), y
dar sus tipos -}

foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB _ x Nil = x
foldAB f x (Bin i r d) = f (foldAB f x i) r (foldAB f x d)

{- recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs) -}

recAB :: (b -> AB a -> AB a -> a -> b -> b) -> b -> AB a -> b
recAB f x Nil = x
recAB f x (Bin i r d) = f (recAB f x i) i d r (recAB f x d)

{- Definir las funciones esNil, altura y cantNodos (para esNil puede utilizarse case en lugar de foldAB
o recAB). -}

esNil :: AB a -> Bool
esNil a = case a of
    Nil -> True
    Bin i r d -> False

altura :: AB a -> Int
altura = foldAB (\ rec1 _ rec2 -> 1 + max rec1 rec2) 0

cantNodos :: AB a -> Int
cantNodos = foldAB (\ rec1 _ rec2 -> 1 + rec1 + rec2) 0

{- Definir la función mejorSegún :: (a -> a -> Bool) -> AB a -> a, análoga a la del ejercicio 3, para árboles.
Se recomienda definir una función auxiliar para comparar la raíz con un posible resultado de la recursión
para un árbol que puede o no ser Nil. -}

mejorSegún :: Num a => (a -> a -> Bool) -> AB a -> a
mejorSegún f = foldAB (\ rec1 r rec2 -> 
                                    if f rec1 r && f rec1 rec2 
                                        then rec1 
                                    else 
                                        if f rec2 r && f rec2 rec1 
                                            then rec2
                                    else r) 0

{- Definir la función esABB :: Ord a => AB a -> Bool que chequea si un árbol es un árbol binario de búsqueda.
Recordar que, en un árbol binario de búsqueda, el valor de un nodo es mayor o igual que los valores que
aparecen en el subárbol izquierdo y es estrictamente menor que los valores que aparecen en el subárbol
derecho -}