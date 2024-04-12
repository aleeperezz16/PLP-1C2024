{E0} elem e [] = False \
{E1} elem e (x : xs) = (e == x) || elem e xs

1. Eq a => $\forall$ xs :: [a], $\forall$ e :: a, elem e xs = elem e (nub xs)

Verdadero

- Caso base: P([])
    - elem e [] = elem e (nub [])
    - False = elem e []
    - False = False

- Paso inductivo: P(xs) => P(x : xs) \
Qvq: elem e (x : xs) = elem e (nub (x : xs))
    - elem e (x : xs)
    - {E1} (e == x) || elem e xs
    - {HI} (e == x) || elem e (nub xs)
    - elem e (x : nub xs)
    - elem e (nub (x : xs))