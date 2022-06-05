import Prelude

type Data = (Int, Int, Int)
bissexto :: Data -> Bool
bissexto (_, _, a) | (mod a 4 == 0) && ((mod a 100 /= 0) || (mod a 400 == 0)) = True
                   | otherwise = False

valida :: Data -> Bool
valida (d,m,a) | m < 1 || m > 12 = False
               | d < 1 || d > 31 = False
               | (bissexto (d,m,a) == True) && (m== 2) && (d > 29) = False
               | (m== 4 || m== 6 || m==9 || m== 11) && (d > 30) = False
               | (bissexto (d,m,a) == False) && (m == 2) && (d > 28) = False
               | otherwise = True

type Hora = (Int, Int, Int)
totalSegundos :: Hora -> Int
totalSegundos (h,m,s) = (h*60*60) + (m*60) + s

