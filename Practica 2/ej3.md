1. $\forall$ xs :: [a], length (duplicar xs) = 2 * length xs
- Caso base: P([])
    - length (duplicar []) = 2 * length []
    - {D0} length [] = 2 * length []
    - {L0} 0 = 2 * 0 = 0
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs)\
Quiero llegar a: length (duplicar (x : xs)) = 2 * length (x : xs)
    - length (duplicar (x : xs)) 
    - {D1} length (x : x : duplicar xs)
    - {L1} 1 + length (x : duplicar xs)
    - {L1} 1 + 1 + length (duplicar xs)
    - {HI} 2 + 2 * length xs
    - {FC} 2 * (1 + length xs) 
    - {L1} 2 * length (x : xs)

2. $\forall$ xs :: [a], $\forall$ ys :: [a], length (append xs ys) = length xs + length ys
- Caso base: P([])
    - length (append [] ys) = length [] + length ys
    - {A0 - L0} length ys = 0 + length ys
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs)\
Quiero llegar a: length (append (x : xs) ys) = length (x : xs) + length ys
    - length (append (x : xs) ys)
    - {A1} length (x : append xs ys)
    - {L1} 1 + length (append xs ys)
    - {HI} 1 + length xs + length ys
    - {L1} length (x : xs) + length ys

{M0} map f [] = [] \
{M1} map f (x : xs) = f x : map f xs

3. $\forall$ xs :: [a], $\forall$ f :: (a -> b), length (map f xs) = length xs
- Caso base: P([]) 
    - length (map f []) = length []
    - {M0} length [] = length []
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs)\
Quiero llegar a: length (map f (x : xs)) = length (x : xs)
    - length (map f (x : xs))
    - {M1} length (f x : map f xs)
    - {L1} 1 + length (map f xs)
    - {HI} 1 + length xs
    - {L1} length (x : xs)

{F0} filter f []    = [] \
{F1} filter f (x : xs)  | f x       = x : filter f xs
                        | otherwise =     filter f xs

{E0} elem e [] = False \
{E1} elem e (x : xs) = (e == x) || elem e xs

4. $\forall$ xs :: [a], $\forall$ p :: a -> Bool, $\forall$ e :: a, (elem e (filter p xs) = True) $\Rightarrow$ (elem e xs = True)
- Caso base: P([])
    - (elem e (filter p []) = True) $\Rightarrow$ (elem e [] = True)
    - {F0} (elem e [] = True) $\Rightarrow$ (elem e [] = True)
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs)\
Quiero llegar a: (elem e (filter p (x : xs)) = True) $\Rightarrow$ (elem e (x : xs) = True)
    - elem e (filter p (x : xs)) = True
    - {F1}
        - {Cond. True} elem e (x : filter p xs) = True
            - {E1} (e == x) || elem e (filter p xs) = True
            - {HI} (e == x) || elem e xs = True
            - {E1} elem e (x : xs)
        - {otherwise} elem e (filter p xs) = True
            - {HI} elem e xs = True
            - {Completo para llegar a E1} (e == x) || elem e xs = True
            - {E1} elem e (x : xs) = True

{FR0} foldr f z [] = z \
{FR1} foldr f z (x : xs) = f x (foldr f z xs)

5. $\forall$ xs :: [a], $\forall$ x :: a, length (ponerAlFinal x xs) = 1 + length xs
- Caso base: P([])
    - length (ponerAlFinal x []) = 1 + length []
    - {P0} length (foldr (:) (x : []) [])
    - {FR0} length (x : [])
    - {L1} 1 + length []
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs)\
Quiero llegar a: length (ponerAlFinal x' (x : xs)) = 1 + length (x : xs)
    - length (ponerAlFinal x' (x : xs))
    - {P0} length (foldr (:) (x' : []) (x : xs))
    - {FR1} length ((:) x (foldr (:) (x' : []) xs))
    - {L1} 1 + length (foldr (:) (x' : []) xs)
    - {P0} 1 + length (ponerAlFinal x' xs)
    - {HI} 1 + 1 + length xs
    - {L1} 1 + length (x : xs)

{FL0} foldl f ac [] = ac \
{FL1} foldl f ac (x : xs) = foldl f (f ac x) xs

6. $\forall$ xs :: [a], $\forall$ x :: a, head (reverse (ponerAlFinal x xs)) = x
- Caso base: P([])
    - head (reverse (ponerAlFinal x []))
    - {P0} head (reverse (foldr (:) (x : []) []))
    - {FR0} head (reverse (x : []))
    - {R0} head (foldl (flip (:)) [] (x : []))
    - {FL1} head (foldl (flip (:)) ((flip (:)) [] x) [])
    - {FL0} head ((flip (:)) [] x)
    - {FLIP} head ((:) x [])
    - {HEAD} head (x : [])
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs) \
Quiero llegar a: head (reverse (ponerAlFinal x' (x : xs))) = x'
    - head (reverse (ponerAlFinal x' (x : xs)))
    - {P0} head (reverse (foldr (:) (x' : []) (x : xs)))
    - {FR1} head (reverse ((:) x (foldr (:) (x' : []) xs)))
    - {P0} head (reverse (x : ponerAlFinal x' xs))
    - {R0} head (foldl (flip (:)) [] (x : ponerAlFinal x' xs))
    - {FL1} head (foldl (flip (:)) ((flip (:)) [] x) (ponerAlFinal x' xs))
    - {FLIP} head (foldl (flip (:)) (x : []) (ponerAlFinal x' xs))

{FL0} foldl f ac [] = ac \
{FL1} foldl f ac (x : xs) = foldl f (f ac x) xs

{FR0} foldr f z [] = z \
{FR1} foldr f z (x : xs) = f x (foldr f z xs)
