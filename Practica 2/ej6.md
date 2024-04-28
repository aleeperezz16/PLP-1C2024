{E0} elem e [] = False \
{E1} elem e (x : xs) = (e == x) || elem e xs

{F0} filter f []    = [] \
{F1} filter f (x : xs)  | f x       = x : filter f xs
                        | otherwise =     filter f xs

1. Eq a => ∀ xs::[a] .∀ e::a .∀ p::a -> Bool .elem e xs && p e = elem e (filter p xs)

Verdadero

- Caso base: P([])
    - elem e [] && p e
    - False = elem e []

- Paso inductivo: P(xs) => P(x : xs) \
qvq: elem e (x : xs) && p e = elem e (filter p (x : xs))
    - {E1} (x == e) || elem e xs && p e
    - {HI} (x == e) || elem e (filter p xs)
    - {E1} elem e (x : filter p xs)
    - {F1 - True} elem e (filter f (x : xs))

2. Eq a => $\forall$ xs :: [a], $\forall$ e :: a, elem e xs = elem e (nub xs)

Verdadero

- Caso base: P([])
    - elem e [] = elem e (nub [])
    - False = elem e []
    - False = False

- Paso inductivo: P(xs) => P(x : xs) \
Qvq: elem e (x : xs) = elem e (nub (x : xs)), sea p = (\y -> x /= y)
    - elem e (nub (x : xs))
    - {N1} elem e (x : filter p (nub xs))
    - {E1} (e == x) || elem e (filter p (nub xs))
    - {1.} (e == x) || elem e (nub xs) && p e
    - {HI} (e == x) || elem e xs && p e
    - {E1} elem e (x : xs) && p e
        - Por 1. esto vale.

3. Eq a => ∀ xs::[a] .∀ ys::[a] .∀ e::a .elem e (union xs ys) = (elem e xs) || (elem e ys)

Verdadero

- Caso base: P([])
    - elem e (union [] ys)
    - {U0} elem e (nub ([] ++ ys))
    - {++} elem e (nub ys)
    - {2.} elem e ys

    - {E1} (elem e []) || (elem e ys)
    - False || (elem e ys)
    - elem e ys

- Paso inductivo: P(xs) => P(x : xs) \
Qvq: elem e (union (x : xs) ys) = (elem e (x : xs)) || (elem e ys)
    - (elem e (x : xs)) || (elem e ys)
    - {E1} ((e == x) || (elem e xs)) || (elem e ys)
    - {||} (e == x) || (elem e xs) || (elem e ys)
    - {HI} (e == x) || elem e (union xs ys)
    - {E1} elem e (x : union xs ys)
    - {U0} elem e (x : nub (xs ++ ys))
    - {1.} elem e (x : (xs ++ ys))
    - {++} elem e ((x : xs) ++ ys)
    - {1.} elem e (nub ((x : xs) ++ ys))
    - {U0} elem e (union (x : xs) ys)

4. Eq a => ∀ xs::[a] .∀ ys::[a] .∀ e::a .elem e (intersect xs ys) = (elem e xs) && (elem e ys)
Sea p = (\e -> elem e ys)

- Caso base: P([])
    - elem e (intersect [] ys) = (elem e []) && (elem e ys)
    - {I0 - E1} elem e (filter p []) = False && (elem e ys)
    - {F0} elem e [] = False
    - {E0} False = False

- Paso inductivo: P(xs) => P(x : xs) \
qvq: elem e (intersect (x : xs) ys) = (elem e (x : xs)) && (elem e ys)
    - elem e (intersect (x : xs) ys)
    - {I0} elem e (filter p (x : xs))
    - {F1 - True}
        - elem e (x : filter p xs)
        - {E1} (e == x) || elem e (filter p xs)
        - {1.} (e == x) || elem e xs && p e
        - {E1} elem e (x : xs) && p e
        - (elem e (x : xs)) && (\e -> elem e ys) e
        - {$\lambda$} (elem e (x : xs)) && (elem e ys)
    - {F1 - otherwise}
        - elem e (filter p xs)
        - {1.} elem e xs && p xs
        - (elem e xs) && (\e -> elem e ys) e
        - {$\lambda$} (elem e xs) && (elem e ys)
            - Esto es cierto por HI

5. Eq a => ∀ xs::[a] .∀ ys::[a] .length (union xs ys) = length xs + length ys

Falso

xs = [1, 2, 3], ys = [1, 2, 2]

6. Eq a => ∀ xs::[a] .∀ ys::[a] .length (union xs ys) ≤ length xs + length ys

Verdadero

Caso base: P([])
    - length (union [] ys) $\leq$ length [] + length ys
    - {U0} length (nub ([] ++ ys)) $\leq$ 0 + length ys
    - {++} length (nub ys) $\leq$ length ys
    - {=> 1.} length ys $\leq$ length ys

Paso inductivo: P(xs) => P(x : xs) \
qvq: length (union (x : xs) ys) $\leq$ length (x : xs) + length ys
    - length (union (x : xs) ys)
    - {U0} length (nub ((x : xs) ++ ys))
    - {=> 1.} length ((x : xs) ++ ys)
    - {++} length (x : (xs ++ ys))
    - {L1} 1 + length (xs ++ ys)
    - {Ejs anteriores} 1 + length xs + length ys