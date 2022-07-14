distancia :: Eq t => [t] ->[t] -> Int
distancia [] []= 0
distancia p1 [] = length p1
distancia [] p2 = length p2
distancia (x1:x1s) (x2:x2s) | x1 == x2 = distancia x1s x2s
                            | otherwise = 1 + distancia x1s x2s

removeDup :: String -> String
removeDup [] = []
removeDup [n] = [n]
removeDup (x:xs) = x:(removeDup $ filter (/=x) xs)


quicksortupla :: [(String,Int)] -> [(String,Int)]
quicksortupla [] = []
quicksortupla (x:xs) = quicksortupla [y | y <- xs , snd y <= snd x] ++ [x] ++ quicksortupla [y | y <- xs , snd y > snd x]

hamming :: Eq t => [[t]] -> [[t]] -> [([t],[t],Int)]
hamming [] [] = []
hamming list1 list2 = [(x,y,(distancia x y))|x <- list1, y <- list2]