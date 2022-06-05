
somar :: IO ()
somar = do
putStrLn "digite o primeiro número:"
s1 <- getLine
putStrLn "digite o segundo número:"
s2 <- getLine
putStrLn "S soma é igual a:"
putStrLn (show ( read s1 + read s2))
