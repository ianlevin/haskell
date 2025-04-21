module Funciones where

fibonacci :: Int -> Int
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci(n-1) + fibonacci (n-2)

medioFact :: Integer -> Integer 
medioFact n | n == 3 = n
            | n == 4 = n*2
            | otherwise = n * medioFact(n - 2)

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | div n 10 == 0 = True
                      | otherwise = mod n 10 == mod (div n 10) 10 && todosDigitosIguales (div n 10)

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos(div n 10)

esCapicua :: Integer -> Bool
esCapicua n | cantDigitos n == 1 = True
            | otherwise = div n (10^((cantDigitos n) -1)) == mod n 10 && esCapicua (mod (div n 10) (10^((cantDigitos(n)-2))))

------------------------------------------------------------------------------------------------------
longitud :: [t] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

ultimo :: [t] -> t
ultimo [t] = t
ultimo (_:xs) = ultimo xs

principio :: [t] -> [t]
principio [t] = []
principio (x:xs) = (x:(principio xs))

reverso :: [t] -> [t]
reverso [] = []
reverso t = ultimo t:reverso (principio t)

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs) | t == x = True
                   | otherwise = pertenece t xs

todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [t] = True
todosIguales (y:x:xs) = y == x && todosIguales (x:xs)

todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [t] = True
todosDistintos (x:xs) = pertenece x xs == False && todosDistintos xs

hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [t] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs
---------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------

sacarBlancosRepetidos :: [Char] -> [Char]
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (y:x:xs) | y == x && x == ' ' = sacarBlancosRepetidos (x:xs)
                               | otherwise = y:sacarBlancosRepetidos (x:xs)
            
borrarEspaciosInicio :: [Char] -> [Char]
borrarEspaciosInicio [] = []
borrarEspaciosInicio (x:xs) | x /= ' ' = (x:xs)     
                            | otherwise = borrarEspaciosInicio xs      
            
borrarEspaciosFin :: [Char] -> [Char]
borrarEspaciosFin = reverse . dropWhile (== ' ') . reverse

borrarEspaciosInicioFin :: [Char] -> [Char]
borrarEspaciosInicioFin x = borrarEspaciosInicio (borrarEspaciosFin x)

sacarEspaciosInnecesarios :: [Char] -> [Char]
sacarEspaciosInnecesarios palabra = sacarBlancosRepetidos (borrarEspaciosInicioFin palabra)

sacarPrimerPalabra :: [Char] -> [Char]
sacarPrimerPalabra [] = []
sacarPrimerPalabra (x:xs) | x /= ' ' = sacarPrimerPalabra xs
                    | otherwise = xs

contarPalabras :: [Char] -> Integer
contarPalabras [] = 0
contarPalabras oracion = 1 + contarPalabras (sacarPrimerPalabra (sacarEspaciosInnecesarios oracion))
--------------------------------------------------------------------------------------------------

obtenerPrimerPalabra :: [Char] -> [Char]
obtenerPrimerPalabra [] = []
obtenerPrimerPalabra (x:xs) | x /= ' ' = x:obtenerPrimerPalabra xs
                            | otherwise = []

palabras :: [Char] -> [[Char]]
palabras [] = []
palabras oracion = obtenerPrimerPalabra (sacarEspaciosInnecesarios oracion):palabras (sacarPrimerPalabra (sacarEspaciosInnecesarios oracion))

masLarga :: [[Char]] -> [Char]
masLarga [] = []
masLarga [x] = x
masLarga (y:x:xs) | longitud y > longitud x = masLarga (y:xs) 
                  | otherwise = masLarga (x:xs)

palabraMasLarga :: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga oracion = masLarga (palabras oracion)
