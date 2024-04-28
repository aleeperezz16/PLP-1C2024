import Test.HUnit

{-- Tipos --}

import Data.Either
import Data.List

data Dirección = Norte | Sur | Este | Oeste
  deriving (Eq, Show)
type Posición = (Float, Float)

data Personaje = Personaje Posición String  -- posición inicial, nombre
  | Mueve Personaje Dirección               -- personaje que se mueve, dirección en la que se mueve
  | Muere Personaje                         -- personaje que muere
  deriving (Eq, Show)
data Objeto = Objeto Posición String        -- posición inicial, nombre
  | Tomado Objeto Personaje                 -- objeto que es tomado, personaje que lo tomó
  | EsDestruido Objeto                      -- objeto que es destruido
  deriving (Eq, Show)
type Universo = [Either Personaje Objeto]

{-- Observadores y funciones básicas de los tipos --}

siguiente_posición :: Posición -> Dirección -> Posición
siguiente_posición p Norte = (fst p, snd p + 1)
siguiente_posición p Sur = (fst p, snd p - 1)
siguiente_posición p Este = (fst p + 1, snd p)
siguiente_posición p Oeste = (fst p - 1, snd p)

posición :: Either Personaje Objeto -> Posición
posición (Left p) = posición_personaje p
posición (Right o) = posición_objeto o

posición_objeto :: Objeto -> Posición
posición_objeto = foldObjeto const (const posición_personaje) id

nombre :: Either Personaje Objeto -> String
nombre (Left p) = nombre_personaje p
nombre (Right o) = nombre_objeto o

nombre_personaje :: Personaje -> String
nombre_personaje = foldPersonaje (const id) const id

está_vivo :: Personaje -> Bool
está_vivo = foldPersonaje (const (const True)) (const (const True)) (const False)

fue_destruido :: Objeto -> Bool
fue_destruido = foldObjeto (const (const False)) const (const True)

universo_con :: [Personaje] -> [Objeto] -> [Either Personaje Objeto]
universo_con ps os = map Left ps ++ map Right os

es_un_objeto :: Either Personaje Objeto -> Bool
es_un_objeto (Left o) = False
es_un_objeto (Right p) = True

es_un_personaje :: Either Personaje Objeto -> Bool
es_un_personaje (Left o) = True
es_un_personaje (Right p) = False

-- Asume que es un personaje
personaje_de :: Either Personaje Objeto -> Personaje
personaje_de (Left p) = p

-- Asume que es un objeto
objeto_de :: Either Personaje Objeto -> Objeto
objeto_de (Right o) = o

en_posesión_de :: String -> Objeto -> Bool
en_posesión_de n = foldObjeto (const (const False)) (\ r p -> nombre_personaje p == n) (const False)

objeto_libre :: Objeto -> Bool
objeto_libre = foldObjeto (const (const True)) (const (const False)) (const False)

norma2 :: (Float, Float) -> (Float, Float) -> Float
norma2 p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

cantidad_de_objetos :: Universo -> Int
cantidad_de_objetos = length . objetos_en

cantidad_de_personajes :: Universo -> Int
cantidad_de_personajes = length . personajes_en

distancia :: (Either Personaje Objeto) -> (Either Personaje Objeto) -> Float
distancia e1 e2 = norma2 (posición e1) (posición e2)

objetos_libres_en :: Universo -> [Objeto]
objetos_libres_en u = filter objeto_libre (objetos_en u)

está_el_personaje :: String -> Universo -> Bool
está_el_personaje n = foldr (\x r -> es_un_personaje x && nombre x == n && (está_vivo $ personaje_de x) || r) False

está_el_objeto :: String -> Universo -> Bool
está_el_objeto n = foldr (\x r -> es_un_objeto x && nombre x == n && not (fue_destruido $ objeto_de x) || r) False

-- Asume que el personaje está
personaje_de_nombre :: String -> Universo -> Personaje
personaje_de_nombre n u = foldr1 (\x1 x2 -> if nombre_personaje x1 == n then x1 else x2) (personajes_en u)

-- Asume que el objeto está
objeto_de_nombre :: String -> Universo -> Objeto
objeto_de_nombre n u = foldr1 (\x1 x2 -> if nombre_objeto x1 == n then x1 else x2) (objetos_en u)

es_una_gema :: Objeto -> Bool
es_una_gema o = isPrefixOf "Gema de" (nombre_objeto o)

{-Ejercicio 1-}

foldPersonaje :: (Posición -> String -> a) -> (a -> Dirección -> a) -> (a -> a) -> Personaje -> a
foldPersonaje f _ _ (Personaje p n) = f p n
foldPersonaje f g h (Mueve p d) = g (foldPersonaje f g h p) d
foldPersonaje f g h (Muere p) = h (foldPersonaje f g h p)

foldObjeto :: (Posición -> String -> a) -> (a -> Personaje -> a) -> (a -> a) -> Objeto -> a
foldObjeto f _ _ (Objeto p n) = f p n
foldObjeto f g h (Tomado o p) = g (foldObjeto f g h o) p
foldObjeto f g h (EsDestruido o) = h (foldObjeto f g h o)

{-Ejercicio 2-}

posición_personaje :: Personaje -> Posición
posición_personaje = foldPersonaje const siguiente_posición id

nombre_objeto :: Objeto -> String
nombre_objeto = foldObjeto (const id) const id

{-Ejercicio 3-}

objetos_en :: Universo -> [Objeto]
objetos_en = map objeto_de . filter es_un_objeto

personajes_en :: Universo -> [Personaje]
personajes_en = map personaje_de . filter es_un_personaje

{-
  {O0} objetos_en = map objeto_de . filter es_un_objeto
  {F0} filter f []    = []
  {F1} filter f (x : xs)  | f x       = x : filter f xs
                          | otherwise =     filter f xs
  {M0} map f [] = []
  {M1} map f (x : xs) = f x : map f xs

  {E0} elem e [] = False
  {E1} elem e (x : xs) = (e == x) || elem e xs

  {OD0} objeto_de (Right o) = o
 -}

-- ∀ u :: Universo . ∀ o :: Objeto . elem o (objetos_en u) ⇒ elem (Right o) u
-- Inducción estructural

-- Caso base: P([]) -> elem o (objetos_en []) ⇒ elem (Right o) []
-- {O0} elem o (map objeto_de . filter es_un_objeto [])
-- {(.)} elem o (map objeto_de (filter es_un_objeto []))
-- {F0} elem o (map objeto_de [])
-- {M0} elem o []
-- False, pero (False ⇒ elem (Right o) u) = True

-- ∀ us :: Universo, ∀ u :: Either Personaje Objeto, ∀ o :: Objeto (P(us) => P(u : us))
-- Quiero ver que: elem o (objetos_en (u : us)) ⇒ elem (Right o) (u : us)
-- {O0} elem o (map objeto_de . filter es_un_objeto (u : us))
-- {(.)} elem o (map objeto_de (filter es_un_objeto (u : us)))
-- {F1 - Caso True}
  -- elem o (map objeto_de (u : filter es_un_objeto us))
  -- {M1} elem o (objeto_de u : map objeto_de (filter es_un_objeto us))
  -- {E1} (o == objeto_de u) || elem o (map objeto_de (filter es_un_objeto us))
  -- {(.) - O0} (o == objeto_de u) || elem o (objetos_en us)
  -- {HI} (o == objeto_de u) || elem (Right o) us
  -- {OD0} (objeto_de (Right o) == objeto_de u) || elem (Right o) us
  -- {Simp. objeto_de} ((Right o) == u) || elem (Right o) us
  -- {E1} elem (Right o) (u : us)
-- {F1 - Caso otherwise}
  -- elem o (map objeto_de (filter es_un_objeto us))
  -- {O0} elem o (objetos_en us)
  -- {HI} elem (Right o) us

{-Ejercicio 4-}

objetos_en_posesión_de :: String -> Universo -> [Objeto]
objetos_en_posesión_de n u = filter (en_posesión_de n) (objetos_en u)

{-Ejercicio 5-}

-- Asume que hay al menos un objeto
mejorSegún :: (a -> a -> Bool) -> [a] -> a
mejorSegún f = foldr1 (\ x acc -> if f x acc then x else acc)

objeto_libre_mas_cercano :: Personaje -> Universo -> Objeto
objeto_libre_mas_cercano p u = mejorSegún (\ x y -> distancia (Right x) (Left p) < distancia (Right y) (Left p)) (objetos_libres_en u)

{-Ejercicio 6-}

tiene_thanos_todas_las_gemas :: Universo -> Bool
tiene_thanos_todas_las_gemas u = length (filter es_una_gema (objetos_en_posesión_de "Thanos" u)) == 6

{-Ejercicio 7-}

podemos_ganarle_a_thanos :: Universo -> Bool
podemos_ganarle_a_thanos u | tiene_thanos_todas_las_gemas u = False
                           | está_el_personaje "Thor" u = está_el_objeto "StormBreaker" u
                           | está_el_personaje "Wanda" u && está_el_personaje "Visión" u = está_el_objeto "Gema de la Mente" u && en_posesión_de "Visión" (objeto_de_nombre "Gema de la Mente" u)
                           | otherwise = False
{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

phil = Personaje (0,0) "Phil"
mjölnir = Objeto (2,2) "Mjölnir"
universo_sin_thanos = universo_con [phil] [mjölnir]

testsEj1 = test [ -- Casos de test para el ejercicio 1
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) phil             -- Caso de test 1 - expresión a testear
    ~=? 0                                                               -- Caso de test 1 - resultado esperado
  ,
  foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Muere phil)     -- Caso de test 2 - expresión a testear
    ~=? 1                                                               -- Caso de test 2 - resultado esperado
  ,
  foldObjeto (\o s -> 0) (\r p -> r+1) (\r -> r+1) mjölnir             -- Caso de test 3 - expresión a testear
    ~=? 0                                                               -- Caso de test 3 - resultado esperado
  ,
  foldObjeto (\o s -> 0) (\r p -> r+1) (\r -> r+1) (EsDestruido mjölnir)     -- Caso de test 4 - expresión a testear
    ~=? 1                                                               -- Caso de test 4 - resultado esperado
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  posición_personaje phil       -- Caso de test 1 - expresión a testear
    ~=? (0,0)                   -- Caso de test 1 - resultado esperado
  ,
  posición_personaje (Mueve phil Norte)       -- Caso de test 2 - expresión a testear
  ~=? (0,1)                   -- Caso de test 2 - resultado esperado
  ,
  posición_personaje (Mueve phil Sur)       -- Caso de test 3 - expresión a testear
  ~=? (0,-1)                   -- Caso de test 3 - resultado esperado
  ,
  posición_personaje (Mueve phil Este)       -- Caso de test 4 - expresión a testear
  ~=? (1,0)                   -- Caso de test 4 - resultado esperado
  ,
  posición_personaje (Mueve phil Oeste)       -- Caso de test 4 - expresión a testear
  ~=? (-1,0)                   -- Caso de test 4 - resultado esperado
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  objetos_en []       -- Caso de test 1 - expresión a testear
    ~=? []            -- Caso de test 1 - resultado esperado
  ,
  objetos_en universo_sin_thanos       -- Caso de test 2 - expresión a testear
  ~=? [mjölnir]            -- Caso de test 2 - resultado esperado
  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  objetos_en_posesión_de "Phil" []       -- Caso de test 1 - expresión a testear
    ~=? []                             -- Caso de test 1 - resultado esperado
  ,
  objetos_en_posesión_de "Phil" (universo_con [phil] [Tomado mjölnir phil])       -- Caso de test 2 - expresión a testear
  ~=? [Tomado mjölnir phil]                             -- Caso de test 2 - resultado esperado
  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5
  objeto_libre_mas_cercano phil [Right mjölnir]       -- Caso de test 1 - expresión a testear
    ~=? mjölnir                                       -- Caso de test 1 - resultado esperado
  ,
  objeto_libre_mas_cercano phil [Right mjölnir, Right (Objeto (1,1) "StormBreaker")]       -- Caso de test 2 - expresión a testear
  ~=? Objeto (1,1) "StormBreaker"                                       -- Caso de test 2 - resultado esperado
  ,
  objeto_libre_mas_cercano phil [Right mjölnir, Right (Objeto (-2,-2) "StormBreaker")]       -- Caso de test 2 - expresión a testear
  ~=? Objeto (-2,-2) "StormBreaker"                                       -- Caso de test 2 - resultado esperado
  ]

thanos = Personaje (0,0) "Thanos"
universon_con_thanos_con_todas_las_gemas = universo_con [thanos] [Tomado (Objeto (0,0) "Gema de la Realidad") thanos, Tomado (Objeto (0,0) "Gema del Espacio") thanos, Tomado (Objeto (0,0) "Gema del Poder") thanos, Tomado (Objeto (0,0) "Gema del Tiempo") thanos, Tomado (Objeto (0,0) "Gema de la Mente") thanos, Tomado (Objeto (0,0) "Gema del Alma") thanos]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  tiene_thanos_todas_las_gemas universo_sin_thanos       -- Caso de test 1 - expresión a testear
    ~=? False                                            -- Caso de test 1 - resultado esperado
  ,
  tiene_thanos_todas_las_gemas (universo_con [thanos] [])
    ~=? False                                            -- Caso de test 2 - resultado esperado
  ,
  tiene_thanos_todas_las_gemas universon_con_thanos_con_todas_las_gemas       -- Caso de test 2 - expresión a testear
    ~=? True                                            -- Caso de test 2 - resultado esperado
  ]

vision = Personaje (0,0) "Visión"

testsEj7 = test [ -- Casos de test para el ejercicio 7
  podemos_ganarle_a_thanos universo_sin_thanos         -- Caso de test 1 - expresión a testear
    ~=? False                                          -- Caso de test 1 - resultado esperado
  ,
  podemos_ganarle_a_thanos universon_con_thanos_con_todas_las_gemas
    ~=? False                                          -- Caso de test 2 - resultado esperado
  ,
  podemos_ganarle_a_thanos (universo_con [thanos, Personaje (0,0) "Thor"] [Objeto (0,0) "StormBreaker"])
    ~=? True                                          -- Caso de test 3 - resultado esperado
  ,
  podemos_ganarle_a_thanos (universo_con [thanos, Personaje (0,0) "Thor"] [mjölnir])
    ~=? False                                          -- Caso de test 3 - resultado esperado
  ,
  podemos_ganarle_a_thanos (universo_con [thanos, Personaje (0,0) "Wanda", vision] [Tomado (Objeto (0,0) "Gema de la Mente") vision])
    ~=? True                                          -- Caso de test 3 - resultado esperado
  ]
