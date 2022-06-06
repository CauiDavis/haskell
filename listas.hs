{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
contavizinhosiguais :: [Char] -> Int
contavizinhosiguais [] = 0
contavizinhosiguais (x:xs) | length (x:xs) == 1 = 0
                           | x == head xs = 1 + contavizinhosiguais xs
                           | otherwise = contavizinhosiguais xs

somalista :: [Int] -> Int
somalista [] = 0
somalista (x:xs) = x + somalista xs

