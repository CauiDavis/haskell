type Data = (Int, Int, Int)

data Registro
  = Contato String String String
  | Compromisso Data String String
  | Vazio
  deriving (Eq, Ord, Show)

imprimir :: [Registro] -> IO ()
imprimir [] = do putStrLn "\nAGENDA VAZIA"
imprimir lista = do
  putStrLn "\n--------AGENDA--------"
  mapM_ print lista

adicionarContato :: [Registro] -> IO [Registro]
adicionarContato dados = do
  putStrLn "\n-------ADICIONAR-------"
  putStrLn "Digite o nome"
  name <- getLine
  putStrLn "Digite o telefone"
  tel <- getLine
  putStrLn "Digite o endereco"
  end <- getLine
  return (Contato name tel end : dados)

adicionarCompromisso :: [Registro] -> IO [Registro]
adicionarCompromisso dados = do
  putStrLn "\n-------ADICIONAR-------"
  putStrLn "Digite o dia"
  dia <- getLine
  putStrLn "Digite o mes"
  mes <- getLine
  putStrLn "Digite o ano"
  ano <- getLine
  putStrLn "Digite o titulo"
  tit <- getLine
  putStrLn "Digite o desc"
  desc <- getLine
  let comp = Compromisso (read dia :: Int, read mes :: Int, read ano :: Int) tit desc
  let res = buscaData dados comp
  print res
  return (res ++ dados)

mesmaData :: Registro -> Registro -> Bool
mesmaData (Compromisso d1 _ _) (Compromisso d2 _ _)
  | d1 == d2 = True
  | otherwise = False
mesmaData _ _ = False

buscaData :: [Registro] -> Registro -> [Registro]
buscaData [] reg = [reg]
buscaData (cabeca : cauda) reg
  | mesmaData cabeca reg = []
  | otherwise = buscaData cauda reg

remover :: [Registro] -> Registro -> [Registro]
remover [] _ = []
remover lista reg = [e|e<-lista, mesmaData e reg/=True]

menu :: [Registro] -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para inserir contato"
  putStrLn "Digite 2 para inserir compromisso"
  putStrLn "Digite 3 remover da agenda"
  putStrLn "Digite 4 para consultar na agenda"
  putStrLn "Digite 5 imprimir"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar -- descarta o Enter
  case opt of
    '1' -> do
      db <- adicionarContato dados
      putStrLn "Adicionado contato"
      menu db
    '2' -> do
      db <- adicionarCompromisso dados
      putStrLn "Adicionado compromisso"
      menu db
    '3' -> do
      putStrLn "Digite o nome"
      tit <- getLine
      let d= read tit::Int
      let res=remover dados (Compromisso (d,d,d) " " " ")
      putStrLn "\nItem removido com sucesso"
      menu res
    '5' -> do
      imprimir dados
      -- putStrLn "\nItem removido com sucesso"
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

-- agenda =[Contato "Tercio" "1234" "Rua da faveira",Compromisso]

-- busca :: (Eq t, Ord t) => Tree t -> t -> Tree t
-- busca Nil _ = Nil
-- busca (Node esq valor dir) x
--   | x == valor = Node Nil valor Nil
--   | valor < x = busca dir x
--   | otherwise = busca esq x

-- -- n1 = Node Nil 1 Nil
-- -- n4 = Node Nil 4 Nil
-- -- n3 = Node Nil 3 n4
-- -- n5 = Node n3 5 Nil
-- -- n2 = Node n1 2 n5

-- n1 = Node Nil 1 Nil

-- n5 = Node Nil 5 Nil

-- n4 = Node Nil 4 n5

-- n3 = Node Nil 3 n4

-- raiz = Node n1 2 n3

-- altura :: (Tree t) -> Int
-- altura Nil = 0
-- altura (Node ae x ad) = 1 + max (altura ae) (altura ad)

-- fatorBal :: (Tree t) -> Int
-- fatorBal Nil = 0
-- fatorBal (Node ae x ad) = (altura ae) - (altura ad)

-- -- balancear::(Tree t)->(Tree t)
-- -- balancear Nil = Nil
-- -- balancear a
-- --   |fatorBal a < -1 = balSimplesEsq a
-- --   |otherwise = a
-- -- esq,dir,val::(Tree t)->(Tree t)
-- esq (Node e x d) = e

-- dir (Node e x d) = d

-- valor (Node e x d) = x

-- balSimplesEsq :: (Tree t) -> (Tree t)
-- balSimplesEsq Nil = Nil
-- balSimplesEsq (Node ae x ad) = (Node ae x (Node (Node esq_d val_d esq_dir_d) valor_dir_d dir_dir_d))
--   where
--     esq_d = esq (ad)
--     val_d = valor (ad)
--     esq_dir_d = esq (dir (ad))
--     valor_dir_d = valor (dir (ad))
--     dir_dir_d = dir (dir (ad))

-- -- balSimplesDir::(Tree t)->(Tree t)
-- -- balSimplesDir Nil = Nil
-- -- balSimplesDir (Node ae x ad) = (Node (Node (Node esq_d val_d esq_dir_d) valor_dir_d dir_dir_d) x ad)
-- --   where
-- --     dir_e=dir(ae)
-- --     val_e=valor(ae)
-- --     dir_esq_e=dir(esq(ae))
-- --     valor_esq_e=valor(esq(ae))
-- --     esq_esq_e=esq(esq(ae))

-- -- (Node ae x  (Node (Node esq(ad) valor(ad) esq(dir(ad))) valor(dir(ad)) dir(dir(ad))  )  )

-- --
