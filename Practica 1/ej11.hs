{- Definir y dar el tipo del esquema de recursión foldNat sobre los naturales. Utilizar el tipo Integer de
Haskell (la función va a estar definida sólo para los enteros mayores o iguales que 0). -}

foldNat :: (Int -> Int -> Int) -> Int -> Int -> Int
foldNat _ x 0 = x
foldNat f x y = f x (foldNat f x (y - 1))

{- Utilizando foldNat, definir la función potencia. -}

potencia :: Int -> Int -> Int
potencia a = foldNat (\ n k -> k * a) 1