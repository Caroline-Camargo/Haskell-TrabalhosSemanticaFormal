data Temperatura = Frio | Calor
   deriving(Eq, Show)
data Estacao = Verao | Outono | Inverno | Primavera
   deriving(Eq, Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio


data Forma = Circulo Float | Retangulo Float Float
   deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r *r
area (Retangulo b a) = b * a


data Arvore = Folha Int | Nodo Int Arvore Arvore
   deriving(Eq,Show)

arv1 :: Arvore
arv1 = Nodo 10 (Nodo 14 (Nodo 1 (Folha 4) (Folha 2)) (Folha 6)) (Folha 9)

somaArvore :: Arvore -> Int
somaArvore (Folha n) = n
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n a1 a2) = Nodo (2*n) (multDoisArvore a1) (multDoisArvore a2)

--- Implementar a função
--- multArvore:: Int -> Arvore -> Arvore
--- que recebe um inteiro e uma árvore, e multiplica todos os valores contidos na árvore pelo inteiro
multArvore:: Int -> Arvore -> Arvore
multArvore num (Folha n) = Folha (num*n)
multArvore num (Nodo n a1 a2) = Nodo (num*n) (multArvore num a1) (multArvore num a2)


--- Implemente a função
--- contaFolhas :: Arvore -> Int
--- que recebe uma árvore e conta quantas folhas existem nessa árvore
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n a1 a2) = contaFolhas a1 + contaFolhas a2


--- Implemente a função
--- contaNodos :: Árvore -> Int
--- que conta quantos Nodos uma árvore possui
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 1
contaNodos (Nodo n a1 a2) = 1 + contaNodos a1 + contaNodos a2


--- Implemente a função:
--- quantasVezes :: Int -> Arvore -> Int
--- que recebe um inteiro e uma árvore, e conta quantas vezes esse inteiro aparece na árvore
quantasVezes :: Int -> Arvore -> Int
quantasVezes num (Folha n)
   | num == n = 1
   | otherwise = 0
quantasVezes num (Nodo n a1 a2)
   | num == n = 1 + (quantasVezes num a1) + (quantasVezes num a2)
   | otherwise = 0 + (quantasVezes num a1) + (quantasVezes num a2)
   

--- A função max do Haskell, recebe dois inteiros e devolve o maior entre eles:
--- Main*> max 4 33
--- 33
--- Main*> max 10 10
--- 10
--- Usando a função max implemente a função:
--- maxArvore :: Arvore -> Int
--- que encontra o maior inteiro em uma árvore

maxInt :: Int -> Int -> Int
maxInt a b
   | a > b = a
   | otherwise = b
   
maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n a1 a2) = maxInt n (maxInt(maxArvore a1) (maxArvore a2))

--- Uma árvore refletida, é uma árvore com todos os seus ramos esquerdos e direitos trocados.
--- Defina a função:
--- refleteArvore :: Arvore -> Arvore
refleteArvore :: Arvore -> Arvore


--- Implementar a função
--- geraLista :: Arvore -> [Int]
--- que transforma uma árvore em uma lista de inteiros. Não importa a ordem dos inteiros
--- na lista, apenas que todos os inteiros dos Nodos e Folhas estejam na lista result
geraLista :: Arvore -> [Int]