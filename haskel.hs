parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = 1 + parteEntera(n-1)

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos(div n 10)

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

fibonacci :: Integer -> Integer
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

{-esDivisible :: Integer -> Integer -> Bool
esDivisible x y | y > x = False
                | y == x = True
                | otherwise = esDivisible(x (2*y)) || esDivisible(x (3*y))-}

esDivisible :: Integer -> Integer -> Bool              
esDivisible x y | x == 0 = True
                | x < 0 = False
                | otherwise = esDivisible (x-y) y

esCapicua :: Integer -> Bool
esCapicua n | cantDigitos n == 1 = True
            | otherwise = div n (10^((cantDigitos n) -1)) == mod n 10 && esCapicua (mod (div n 10) (10^((cantDigitos(n)-2))))