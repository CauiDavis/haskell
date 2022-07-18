
data Arvore a = Nulo | No (Arvore a) a (Arvore a)
               deriving (Show)
criar_arvore :: (Ord a) => [a] -> Arvore a
criar_arvore [] = Nulo
criar_arvore (x:xs) = criar_arvore_aux (No Nulo x Nulo) xs
      where
           criar_arvore_aux arvore [] = arvore
           criar_arvore_aux arvore (y:ys) = criar_arvore_aux (inserir arvore y) ys

inserir :: (Ord a) => Arvore a -> a -> Arvore a
inserir Nulo x = No Nulo x Nulo
inserir (No arv1 v arv2) x | (v == x) = No arv1 v arv2
                           | (v < x) = No arv1 v (inserir arv2 x)
                           | (v > x) = No (inserir arv1 x) v arv2

--buscar :: (Ord a) => [a] -> a -> [a]
--buscar [] n = []
--buscar listb n = [x | x <- listb, x == n]

--buscararvore ::(Ord a) => [a] -> a -> Arvore a
--buscararvore listba m  = criar_arvore (buscar listba m)

--atualizar :: (Ord a) => [a] -> [a] -> Arvore a
--atualizar n m = criar_arvore (n ++ m)

--total :: [(String,Integer,Double)] -> String
--total [] = "R$:0.0"
--total listt = "R$:" ++ show (sum [x | (_,_,x) <- listt])

multiplicacao :: Float -> Float -> Float
multiplicacao x y = x*y

data Registro = Produto String Float Float | Vazio
              deriving (Eq, Ord, Show)

adicionarProduto :: [Registro] -> IO [Registro]
adicionarProduto dados = do
  putStrLn "\n-------ADICIONAR-------"
  putStrLn "Digite o nome"
  nome <- getLine
  putStrLn "Digite a quantidade"
  qtd <- getLine
  putStrLn "Digite o valor"
  valor <- getLine
  return (Produto nome (read qtd) (read valor) : dados)

mesmoNome :: Registro -> Registro -> Bool
mesmoNome (Produto p1 _ _) (Produto p2 _ _)
  | p1 == p2 = True
  | otherwise = False
mesmoNome _ _ = False

subtraiProduto :: Registro -> Registro -> Registro
subtraiProduto (Produto p1 n1 o1) (Produto _ n2 _) = Produto p1 (n1 - n2) o1

estoque :: Registro -> Bool
estoque (Produto _ x _) | x < 1 = True
                      | otherwise = False

venderProduto :: [Registro] -> Registro -> [Registro]
venderProduto [] _ = []
venderProduto (x:xs) n |  mesmoNome x n == True = [(subtraiProduto x n)] ++ venderProduto xs n
                       |  otherwise = [(x)] ++ venderProduto xs n

vender :: [Registro] -> [Registro]
vender [] = []
vender listv = [x | x <- listv, estoque x == False]

atualizar ::[Registro] -> IO ()
atualizar [] = do putStrLn "\n REGISTRO VAZIO"
atualizar lista = do
  putStrLn "\n--------REGISTRO--------"
  print (show (criar_arvore lista))

--remover :: [Registro] -> Registro -> [Registro]
--remover [] _ = []
--remover lista reg = [e|e<-lista, mesmoNome e reg /= True]

total :: [Registro] -> IO ()
total [] = do 
  putStrLn "\n--------TOTAL--------"
  putStrLn "R$:0.0"
total list = do
  putStrLn "\n--------TOTAL--------"
  print ("R$:" ++ show (sum [multiplicacao x y | Produto _ x y <- list]))

buscar :: [Registro] -> Registro -> IO ()
buscar [] _ = do 
  putStrLn "\n REGISTRO VAZIO PARA BUSCA"
buscar list x = do
  putStrLn "\n--------PRODUTO ENCONTRADO--------"
  print (show ([e|e<-list, mesmoNome e x == True]))

menu :: [Registro] -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para inserir Produto"
  putStrLn "Digite 2 vender Produto"
  putStrLn "Digite 3 atualizar Produtos"
  putStrLn "Digite 4 o valor total"
  putStrLn "Digite 5 buscar Produto"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar -- descarta o Enter
  case opt of
    '1' -> do
      db <- adicionarProduto dados
      putStrLn "Produto Adicionado"
      menu db
    '2' -> do
      putStrLn "\n--------VENDER PRODUTO--------"
      putStrLn "Digite o nome do produto"
      nome <- getLine
      putStrLn "Digite a quantidade"
      qtd <- getLine
      let res1 = venderProduto dados (Produto nome (read qtd) 0.0)
      let res2 = vender res1
      putStrLn "\nProduto vendido com sucesso"
      menu res2
    '3' -> do
      atualizar dados
      menu dados
    '4' -> do
      total dados
      menu dados
    '5' -> do
      putStrLn "\n--------BUSCAR--------"
      putStrLn "digite o nome do produto"
      nome <- getLine
      buscar dados (Produto nome 0.0 0.0)
      menu dados
    '0' -> do
      putStrLn "\n--------FIM--------"
    _ -> do
      putStrLn "\nOpção inválida!"
      menu dados

main :: IO ()
main = do
  menu []
  return ()