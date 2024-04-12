1. flip . flip = id    
- f :: a -> b -> c, $\forall$ x :: a, $\forall$ y :: b,
- flip . flip f x y = flip (flip f x y) = flip f y x = f x y = id f x y

2. $\forall$ f :: (a, b) -> c, uncurry (curry f) = f
- $\forall$ x :: a, $\forall$ y :: b
- uncurry (curry f) (x, y) = curry f x y = f (x, y) = f

3. flip const = const id
- $\forall$ x :: a, $\forall$ y :: b
- flip const x y = const y x = y = id y = const id x y

4. $\forall$ f :: a -> b, $\forall$ g :: b -> c, $\forall$ h :: c -> d, 
((h . g) . f) = (h . (g . f))
$\forall$ x :: a
- ((h . g) . f) x = (h . g) (f x) = h (g (f x)) = h ((g . f) x) = (h . (g . f))