--primeira questao
primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo n|(length[x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
       |otherwise = True

en_primo_aux :: Int -> Int -> Int -> Int
en_primo_aux n m o | primo m == True && o < n = en_primo_aux n (m+1) (o+1)
                   | primo m == False && o < n = en_primo_aux n (m+1)  o
                   | n == o = m -1
en_primo :: Int -> Int
en_primo 1 = 2
en_primo 2 = 3
en_primo n | otherwise = en_primo_aux n 1 0

multilista_en_primo :: Int -> Int
multilista_en_primo n = product [(en_primo x) | x <- [1..n]]

afortunado_aux :: Int -> Int -> Int
afortunado_aux n m | primo ((multilista_en_primo n) + m) == False = afortunado_aux n (m+1)
                   | otherwise = m
afortunado :: Int -> Int
afortunado n = afortunado_aux n 2

--segunda questao
enlista :: String -> Int -> Char
enlista n m | (length n) < m  = '*'
            | m < 0 = '*'
            | otherwise = n !! m

getByIndex :: [Int] -> String -> String
getByIndex listb strb = [x | y <- listb, x <- [(enlista strb y)]]

--terceira questao
pascal :: Int -> [(Int, Int)]
pascal 0 = [(0,0)]
pascal x = [(x,m) | m <-[0..x] ]