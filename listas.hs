{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Move guards forward" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use null" #-}

contavizinhosiguais :: [Char] -> Int
contavizinhosiguais [] = 0
contavizinhosiguais (x:xs) | length (x:xs) == 1 = 0
                           | x == head xs = 1 + contavizinhosiguais xs
                           | otherwise = contavizinhosiguais xs

somalista :: [Int] -> Int
somalista [] = 0
somalista (x:xs) = x + somalista xs

impares :: [Int]
impares = [1,3..100]

pares :: [Int]
pares = [10,12..100]

imparesn :: Int -> [Int]
imparesn 0 = []
imparesn n = [x | x <- [1,3..n]]

multi3e5 :: Int -> [Int]
multi3e5 n = [x | x <- [1..n],mod x 3 == 0,mod x 5 ==0]

tq :: Int -> [(Int,Int)]
tq n = [(x,x*x) | x <-[1..n]]

indices :: [(Int,Int)]
indices = [(x,y) | x <- [1..3],y <-[1..4]]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
listafib :: Int -> [Int]
listafib 0 = []
listafib n = [fib x | x <- [1..n]]

fatores :: Int -> [Int]
fatores n = [x | x <- [1..(div n 2)], mod n x == 0]

auxnumperfeito :: Int -> [Int]
auxnumperfeito num = [ x | x <- [1 .. num-1], ((mod num x) == 0) ]
eh_perfeito :: Int -> Bool
eh_perfeito n | (sum (auxnumperfeito n)) == n = True
              | otherwise = False
listanumperfeito :: Int -> [Int]
listanumperfeito n = [x | x <- [1..n],eh_perfeito x == True]

concatena :: [[Int]] -> [Int]
concatena l = [y | x <- l,y <- x]

type Funcionario = (String, Float)

-- Base de dados
folha :: [Funcionario]
folha = [("Ana", 772.25), ("Jose", 2375.0), ("Caui", 1778.5)]

--Função auxiliar que descompacta um funcionario e retorna somente seu salario
salario :: Funcionario -> Float
salario (_, sal) = sal

-- =================================
-- 1 quanto gasta mensalmente em salarios?
--Usando somente os conceitos de lista (feito em sala)
gastoMensal :: [Funcionario] -> Float
gastoMensal (x:xs)
    |xs == [] = salario x
    |otherwise =  salario (x) + gastoMensal (xs) 

acimasalario :: Float -> [Funcionario]
acimasalario n = [(x,y) | (x,y) <- folha, y > n]

diff :: Float -> [Funcionario]
diff n = [(x, abs(y-n)) |(x,y) <- folha]
aproxsalario :: Float -> Float
aproxsalario n = minimum [y |(_,y) <- diff n]

maissalario :: [Funcionario] -> Float
maissalario (x:xs) | xs == [] = 0
                   | salario x > 1100 && salario x < 2203 = (salario x)*0.09 + maissalario xs
                   | salario x > 2203 && salario x < 3505 = (salario x)*0.12 + maissalario xs
                   | salario x > 3505 && salario x < 6433 = (salario x)*0.14 + maissalario xs
                   | salario x > 6433 = (salario x)*0.22 + maissalario xs
                   | otherwise = maissalario xs

listaper :: [Int]
listaper = [1,2,3,4,5,6,7,9,8]

plistaaux :: Int -> [Int]
plistaaux n = [x | x <- listaper, x == n]
plista :: Int -> Bool
plista n | plistaaux n == [] = False
         | otherwise = True

maiorlista :: [Int] -> Int
maiorlista n = maximum [x | x <- listaper]

enlista :: Int -> Int
enlista n  = listaper !! n

droplista :: Int -> [Int]
droplista n = [x | x <- listaper,(listaper !! n) /= x]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

duplica :: [Int] -> [Int]
duplica lduplica = [x | x <- quicksort (lduplica ++ lduplica)]

reg :: [(Int, String)]
reg = [(15,"Ana"),(22,"Pedro"),(2,"Maria"),(12,"Joao"),(14,"Pablo"),(23,"Poliana")]
quicksortupla :: [(Int,String)] -> [(Int,String)]
quicksortupla [] = []
quicksortupla (x:xs) = quicksortupla [y | y <- xs , fst y <= fst x] ++ [x] ++ quicksortupla [y | y <- xs , fst y > fst x]

add_fim :: [Int] -> Int -> [Int]
add_fim list n = list ++ [n]

hanoi :: Int -> Int -> Int -> Int -> [String]
hanoi 0 _ _ _ = []
hanoi n orig aux dest | n == 1 = [show (orig) ++ "->" ++ show (dest)]
                      | otherwise = (hanoi (n-1) orig dest aux) ++ [show (orig) ++ "->" ++ show (dest)] ++ (hanoi (n-1) aux orig dest)

palidromo :: String -> Bool
palidromo [] = False
palidromo str | str == reverse str = True
              | otherwise = False

intercede :: [Int] -> [Int] -> [Int]
intercede [] [] = []
intercede [] [n] = []
intercede [m] [] = []
intercede (x:xs) (y:ys) | x == y = [x] ++ intercede xs ys
                        | otherwise = intercede xs ys

separa :: [Int] -> Int -> [([Int], [Int])]
separa [] n = []
separa listse n = [((take n listse) , (drop n listse))] 

doisindices :: String -> Int -> Int -> String
doisindices [] n m = []
doisindices listadois n m = take (abs (n-m)) (drop n listadois)

intercala :: [Int] -> [Int] -> [Int]
intercala [] [] = []
intercala [] [n] = []
intercala (x:xs) (y:ys) = x:y:intercala xs ys

passagemaux :: (Int,Float) -> Float
passagemaux n | fst n >= 60 = (snd n)*0.60
              | fst n > 10 && fst n < 60 = (snd n)
              | fst n > 2 && fst n <= 10 = (snd n)*0.50
              | fst n > 0 && fst n <= 2 = (snd n)*0.10
              | otherwise = 0.0
passagem :: [(Int,Float)] -> Float
passagem [] = 0.0
passagem listn = sum [passagemaux x |x <- listn]

imc1 :: (Int,Float,Float) -> (Int,Float)
imc1 (x,y,z) = (x,(y/(z*z)))
imc2 :: (Int,Float) -> (Int,String)
imc2 (x,y) | y < 18.5 = (x,"abaixo do peso")
           | y >= 18.5 && y < 25 = (x,"peso normal")
           | y >= 25 && y < 30 = (x,"excesso de peso")
           | y >= 30 = (x,"obesidade")
classsifica :: [(Int,Float,Float)] ->[(Int,String)]
classsifica [] = []
classsifica listimc = [imc2 (imc1 x) | x <- listimc]