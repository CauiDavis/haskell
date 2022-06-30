analiseaux1 :: String -> Char -> Int
analiseaux1 [] n = 0
analiseaux1 listas n = length [y | y <- listas, y == n]
analise :: String -> [(Char, Int)]
analise lista = [(x,y) | x <- lista, y <- [analiseaux1 lista x]]

quicksortupla :: [(Char,Int)] -> [(Char,Int)]
quicksortupla [] = []
quicksortupla (x:xs) = quicksortupla [y | y <- xs , snd y <= snd x] ++ [x] ++ quicksortupla [y | y <- xs , snd y > snd x]

repeticao :: String -> String
repeticao [] = []
repeticao (x:xs) = [y | y <- xs , y /= x] ++ repeticao xs

ordefreqaux :: String -> [(Char,Int)]
ordefreqaux [] = []
ordefreqaux listt = reverse (quicksortupla (analise listt))
ordefreq :: String -> String
ordefreq [] = []
ordefreq list2 = [x | (x,_) <- ordefreqaux list2]