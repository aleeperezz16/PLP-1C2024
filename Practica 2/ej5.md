
{FR0} foldr f z [] = z \
{FR1} foldr f z (x : xs) = f x (foldr f z xs)

Demostrar que zip = zip' utilizando inducción estructural y el principio de extensionalidad. \

$\forall$ xs :: [a], $\forall$ ys :: [b], zip xs ys = zip' xs ys

- Caso base: P([])
    - zip [] ys
    - {Z0} foldr (\ x rec ys' -> 
                    if null ys' 
                        then [] 
                        else (x, head ys') : rec (tail ys'))
                (const []) [] ys
    - {FR0} const [] ys = []
    - {Z'0} zip' [] ys = []
- Paso inductivo: $\forall$ x :: a, $\forall$ xs :: [a], qvq P(xs) => P(x : xs) \
Quiero llegar a: zip (x : xs) ys = zip' (x : xs) ys
    - zip (x : xs) ys
    - {Z0} foldr (\ x' rec ys' ->
                    if null ys'
                        then []
                        else (x', head ys') : rec (tail ys'))
                (const []) (x : xs) ys
    - {FR1} (\ x' rec ys' ->
                if null ys'
                    then []
                    else (x', head ys') : rec (tail ys')) 
                x (foldr (\ x' rec ys' ->
                    if null ys'
                        then []
                        else (x', head ys') : rec (tail ys'))
                    (const []) xs) ys
    - {Z0} (\ x' rec ys' ->
                if null ys'
                    then []
                    else (x', head ys') : rec (tail ys'))
                x (zip xs) ys
    - {$\lambda$} if null ys then [] else (x, head ys) : zip xs (tail ys)
    - {HI} if null ys then [] else (x, head ys) : zip' xs (tail ys)
    - {Z'1} zip' (x : xs) ys