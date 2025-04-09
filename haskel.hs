todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a, b) (c, d) = (a<c && b<d)

type Punto2D = (Float, Float)

todoMenor2 :: Punto2D -> Punto2D -> Bool
todoMenor2 a b = (fst a < fst b && snd a < snd b)

posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (a, b, c) | mod a 2 == 0 = 1
                       | mod b 2 == 0 = 2
                       | mod c 2 == 0 = 3
                       | otherwise = 4

bisiesto :: Int -> Bool
bisiesto n = mod n 4 == 0 && (mod n 100 /= 0 || mod n 400 == 0)

type Tripla = (Float, Float, Float)

distanciaManhatan :: Tripla -> Tripla -> Float
distanciaManhatan (a,b,c) (d,e,f) = abs (a-d) + abs (b-e) + abs (c-f)

sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x = (mod (div (abs x) 10) 10 + mod (abs x) 10)

comparar :: Int -> Int-> Int
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             |otherwise = 0
