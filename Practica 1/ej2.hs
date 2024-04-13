sum2 :: Num a => [a] -> a
sum2 = foldr (+) 0

elem2 :: Eq a => a -> [a] -> Bool
elem2 n = foldr (\ x acc -> x == n || acc) False

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f = foldr (\ x acc -> if f x then x : acc else acc) []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\ x acc -> (f x) : acc) []

mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\ x acc -> if f x acc then x else acc)

sumasParciales :: Num a => [a] -> [a]
sumasParciales = reverse . foldl (\ ac x -> (if null ac then x else x + head ac) : ac) []

sumaAlt :: Num a => [a] -> a
sumaAlt = sum . foldr (\ x acc -> (if odd (length acc) then x else (-x)) : acc) []

sumaAltInv :: Num a => [a] -> a
sumaAltInv = sum . foldr (\ x acc -> (if even (length acc) then x else (-x)) : acc) []