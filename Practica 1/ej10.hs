generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

{- Usando generate, definir generateBase::([a] -> Bool) -> a -> (a -> a) -> [a], similar a
generate, pero con un caso base para el elemento inicial, y una función que, en lugar de calcular el siguiente
elemento en base a la lista completa, lo calcula a partir del último elemento. Por ejemplo: generateBase
(\l->not (null l) && (last l > 256)) 1 (*2) es la lista las potencias de 2 menores o iguales que 256. -}

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase f x g = x : generate f (\ xs -> g (if null xs then x else last xs))

{- Usando generate, definir factoriales::Int -> [Int], que dado un entero n genera la lista de los
primeros n factoriales. -}
factoriales :: Int -> [Int]
factoriales n = generate (\ l -> length l > n) (\ xs -> if null xs then 1 else last xs * (length xs + 1))

{- Usando generateBase, definir iterateN :: Int -> (a -> a) -> a -> [a] que, toma un entero n, una
función f y un elemento inicial x, y devuelve la lista [x, f x, f (f x), ..., f ( ...(f x) ...)] de
longitud n. Nota: iterateN n f x = take n (iterate f x). -}
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\ l -> length l >= n) x f

-- Redefinir generateFrom usando iterate y takeWhile.
generateFrom' :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom' stop next xs = last (takeWhile (not . stop) (iterate (\ xs -> xs ++ [next xs]) xs))