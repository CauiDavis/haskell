
produtos = [("coxinha",3,345.10),("bomba",2,250.5),("pastel",4,423.10)]
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

buscar :: (Ord a) => [a] -> a -> [a]
buscar [] n = []
buscar listb n = [x | x <- listb, x == n]

buscararvore ::(Ord a) => [a] -> a -> Arvore a
buscararvore listba m  = criar_arvore (buscar listba m)

atualizar :: (Ord a) => [a] -> [a] -> Arvore a
atualizar n m = criar_arvore (n ++ m)

total :: [(String,Integer,Double)] -> String
total [] = "R$:0.0"
total listt = "R$:" ++ show (sum [x | (_,_,x) <- listt])

data Registro = Produto String Integer Double | Vazio
              deriving (Eq, Ord, Show)