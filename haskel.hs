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

{-
problema sumaPotencias(q:N, n:N, m:N): N{

    requiere:{q > 0, n > 0, m > 0}

    asegura:{resultado = }

}


-}
sumatoriaInterna :: Integer -> Integer -> Integer -> Integer
sumatoriaInterna _ _ 0 = 0
sumatoriaInterna q i j = q^(i+j) + sumatoriaInterna q i (j-1)

sumatoriaDoble :: Integer -> Integer -> Integer -> Integer
sumatoriaDoble _ 0 _ = 0
sumatoriaDoble q n m = sumatoriaInterna q n m + sumatoriaDoble q (n-1) m

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m = sumatoriaDoble q n m

esDivisible :: Integer -> Integer -> Integer
esDivisible n divisor | mod n divisor == 0 = divisor
                      | otherwise = esDivisible n (divisor + 1) 

menorDivisor :: Integer -> Integer
menorDivisor n = esDivisible n 2 

{-
esPrimo :: Integer -> Bool
esPrimo 

nesimoPrimo :: Integer -> Integer
nesimoPrimo n = 



primoDivisor :: Integer -> Integer -> Integer
primoDivisor n nprimo = mod n (nesimoPrimo nprimo) == 0 = (nesimoPrimo nprimo)

menorDivisor :: Integer -> Integer
menorDivisor n = primoDivisor n 1
-}

esPrimo :: Integer -> Bool

nEsimoPrimoDesde :: Integer -> Integer -> Integer
nEsimoPrimoDesde desde n 

sumatoriaHastaNPrimosDesde :: Integer -> Integer -> Integer
sumatoriaHastaNPrimosDesde n n = nEsimoPrimoDesde n
sumatoriaHastaNPrimosDesde n desde = nEsimoPrimoDesde desde + sumatoriaHastaNPrimosDesde n (desde + 1)

sumaInicialDePrimos :: Integer -> Integer -> Bool
sumaInicialDePrimos n nprimo | (sumatoriaHastaNPrimosDesde nprimo 1) > n = False
                             | n == (sumatoriaHastaNPrimosDesde nprimo 1) = True
                             | otherwise = sumaInicialDePrimos n (nprimo + 1)



esSumaInicialDePrimos :: Integer -> Bool
esSumaInicialDePrimos n = sumaInicialDePrimos n 1





