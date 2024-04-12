-- No es recursión estructural porque no recorre la cola de la lista como se debe.

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x : xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

-- Si es recursión estructural

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x : xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

entrelazar' :: [a] -> [a] -> [a]
entrelazar' = foldr (\ x recc y -> x : if null y then recc [] else head y : recc (tail y)) id