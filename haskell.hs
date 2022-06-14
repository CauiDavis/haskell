
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Redundant guard" #-}
module Haskell where
  soma :: Int -> Int -> Int
  soma x y = x+y

  subtracao :: Int -> Int -> Int
  subtracao x y = x-y

  multiplicacao :: Float -> Float -> Float
  multiplicacao x y = x*y

  divisao:: Float -> Float -> Float
  divisao x y = x/y

  fat :: Int -> Int
  fat 0 = 1
  fat n|n > 0 = n*fat(n-1)

  primo :: Int -> Bool
  primo 1 = False
  primo 2 = True
  primo n|(length[x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
         |otherwise = True

  mdc :: Int -> Int -> Int
  mdc x y | x > y = mdcaux x y y
        | otherwise = mdcaux y x x
  mdcaux :: Int -> Int -> Int -> Int
  mdcaux x y z | (mod x z) == 0 && (mod y z) == 0 = z
               | otherwise = mdcaux x y(z-1)

  mmc :: Int -> Int -> Int
  mmc x y = div(x*y) (mdc x y)

  potencia :: Int -> Int -> Int
  potencia x 0 = 1
  potencia x 1 = x
  potencia x y| y > 1 = x * potencia x (y-1)

  fib :: Int -> Int
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2)

  fibprimo :: Int -> Bool
  fibprimo n| primo (fib n) == True = True
            | otherwise = False

  auxnumperfeito :: Int -> [Int]
  auxnumperfeito num = [ x | x <- [1 .. num-1], ((mod num x) == 0) ]
  eh_perfeito :: Int -> Bool
  eh_perfeito n | ((sum (auxnumperfeito n)) == n) = True
                | otherwise = False

  num_con_colatz :: Int -> Int
  num_con_colatz 1 = 0
  num_con_colatz n | (mod n 2) == 1 = 1 + num_con_colatz (3*n + 1)
                 | (mod n 2) == 0 = 1 + num_con_colatz (div n 2)
  sp_con_colatz :: Int -> Int 
  sp_con_colatz 1 = 0
  sp_con_colatz 2 = 2
  sp_con_colatz n | primo n == True && mod n 2 == 1 = n + sp_con_colatz (3*n + 1)
                  | mod n 2 == 0 =  sp_con_colatz (div n 2)

  pot :: Int -> Int -> Int
  pot n 0 = 1
  pot n 1 = n
  pot n m  = n^m

  algarismo :: Int -> Int
  algarismo n | n >=0 && n < 10 = 1
              | otherwise = 1 + algarismo (div n 10)

  fr_aux :: Int -> Int -> Int
  fr_aux  n m | fat m < n = 1 + fr_aux n (m+1)
              | fat m == n = 1
  fatorial_reverso :: Int -> Int
  fatorial_reverso 1 = 1
  fatorial_reverso 2 = 2
  fatorial_reverso n | otherwise = fr_aux n 1

  en_primo_aux :: Int -> Int -> Int -> Int
  en_primo_aux n m o | primo m == True && o < n = en_primo_aux n (m+1) (o+1)
                     | primo m == False && o < n = en_primo_aux n (m+1)  o
                     | n == o = m -1
  en_primo :: Int -> Int
  en_primo 1 = 2
  en_primo 2 = 3
  en_primo n | otherwise = en_primo_aux n 1 0

  en_fib_primo :: Int -> Int
  en_fib_primo 1 = 2
  en_fib_primo n  = fib (en_primo n)

  resto :: Int -> Int
  resto n | n == 1 = 1
          | n == 0 = 0
          | n > 1 = resto (n-2)

  div_int :: Int -> Int -> Int
  div_int n m | n == 0 = 0
              | n >= m = 1 + div_int (n-m) m
              | otherwise = 0

  soma_fat :: Int -> Int
  soma_fat 0 = 1
  soma_fat n | n > 0 = fat n + soma_fat (n-1)

  soma_fib :: Int -> Int
  soma_fib 0 = 0
  soma_fib n | n > 0 = fib n + soma_fib (n-1)

  soma_fat_alt :: Int -> Int
  soma_fat_alt 0 = 1
  soma_fat_alt n | n > 0 && mod n 2 == 1 = - fat n + soma_fat_alt (n-1)
                 | n > 0 && mod n 2 == 0 = fat n + soma_fat_alt (n-1) 

  coleta :: Int -> Int
  coleta 7 = 1
  coleta 21 = 2
  coleta 42 = 3
  coleta 70 = 4
  coleta 105 = 5
  coleta 147 = 6
  coleta n = 0


  gold_bach_aux :: Int -> Int -> [Int]
  gold_bach_aux n m | primo n == False || primo m == False = gold_bach_aux (n-1) (m+1)
                    | otherwise = [n,m]
  gold_bach :: Int -> [Int]
  gold_bach 4 = [2,2]
  gold_bach 6 = [3,3]
  gold_bach n = gold_bach_aux (div n 2) (div n 2)

  totiente_aux :: Int -> Int -> Int
  totiente_aux n 1 = 0
  totiente_aux n m | m > 1 && mdc n m == 1 = 1 + totiente_aux n (m-1)
                   | otherwise = totiente_aux n (m-1)
  totiente :: Int -> Int
  totiente 3 = 2
  totiente 6 = 2 
  totiente n = 1 + totiente_aux n (n-1)

  pg :: Int -> Int -> Int -> [Int]
  pg a1 q en = [a1,q,(a1*(potencia q (en-1)))]

  primo_gemeos :: Int -> Int -> Bool
  primo_gemeos n m | ((n+2)==m || (n-2)== m) && primo m == True && primo n == True = True
                 | otherwise = False

  pertence_pg :: Int -> Bool
  pertence_pg n | n <= 2 = False
                | otherwise = aux_ppg n n
  aux_ppg :: Int -> Int -> Bool
  aux_ppg n m | primo n == True && (primo (m+2) == True || primo (m-2) == True) = True
              | otherwise = False 

  cont_pg :: Int -> Int
  cont_pg n | n <= 5 = 0
            | otherwise = aux_cpg n 3 5
  aux_cpg :: Int -> Int -> Int -> Int
  aux_cpg n m o | (m < n && o < n) && primo_gemeos m o == True = 1 + aux_cpg n (m+1) (o+1)
                | (m < n && o < n) && primo_gemeos m o == False = aux_cpg n (m+1) (o+1)
                | otherwise = 0

  soma_pg :: Int -> Int
  soma_pg n | n <= 5 = 0
            | otherwise = aux_spg n 3 5
  aux_spg :: Int -> Int -> Int -> Int
  aux_spg n m o  | (n > m && n > o) && primo_gemeos m o == True = m + o + aux_spg n (m+1) (o+1)
                 | (n > m && n > o) && primo_gemeos m o == False = aux_spg n (m+1) (o+1)
                 | otherwise = 0

