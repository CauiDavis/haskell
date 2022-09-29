--primeira questao 

sequencia1 :: String -> Char -> Int
sequencia1 [] n = 1
sequencia1 (x:xs) n | x == n = 1 + sequencia1 xs x
                    | otherwise = 1
sequencia2 :: String -> Char -> String
sequencia2 [] n = []
sequencia2 (x:xs) n | x == n = [x] ++ sequencia2 xs x
                    | otherwise = []
sequencia :: String -> String
sequencia [] = []
sequencia (x:xs) = [x] ++ sequencia2 xs x ++ show (sequencia1 xs x + 1)

repete :: String -> String
repete [] = []
repete (x:xs) = [x] ++ show (sequencia1 xs x) ++ repete xs 

--segunda questao

analiseaux1 :: (Eq t, Num a) => [t] -> t -> a
analiseaux1 [] n = 0
analiseaux1 (x:xs) n | x == n = 1 + analiseaux1 xs n
                     | otherwise = analiseaux1 xs n
listarepete :: Eq t => Num a => [t] -> [(t,a)]
listarepete lista = [(x,y) | x <- lista, y <- [analiseaux1 lista x]]