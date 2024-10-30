--- Insert Sort
ins :: Int -> [Int] -> [Int]
ins a [] = [a]
ins a (x:xs)
    | a <= x = a:x:xs
    | otherwise = x: ins a xs

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = ins x (iSort xs)


--- Implementar a função pegaPosicao
--- > pegaPosiao 3 [10,2,11,4,5] 
--- 11
pegaPosicao :: Int -> [Int] -> Int
pegaPosicao 1 (x:xs) = x
pegaPosicao idx (x:xs) = pegaPosicao (idx-1) xs


--- Implementar a função pega:
--- > pega 3 [1,2,3,4,5]
-- [1,2,3]
pega :: Int -> [Int] -> [Int]
pega num (x:xs)
    | num == 0 = []
    | otherwise = x : pega (num-1) xs

--- Implementar a função retira:
--- > retira 3 [1,2,3,4,5]
--- [4,5]
retira :: Int -> [Int] -> [Int]
retira num (x:xs)
    | num == 0 = (x:xs)
    | otherwise = retira (num-1) xs


--- Implementar a função mediaLista que cacula a média dos elementos de uma lista
somaLista :: [Float] -> Float
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

quantidadeElementosLista :: [Float] -> Float
quantidadeElementosLista [] = 0
quantidadeElementosLista (x:xs) = 1 + quantidadeElementosLista xs

mediaLista :: [Float] -> Float
mediaLista [] = 0
mediaLista (x:xs) = somaLista (x:xs) / quantidadeElementosLista (x:xs) 


--- Implementar a função pegaMaiores
--- > pegaMaiores 3 [10,2,3,4,5]
--- [10,4,5]
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista (x:xs) = inverteLista xs ++ [x]

inverteListaOrdenada :: [Int] -> [Int]
inverteListaOrdenada (x:xs) = inverteLista(iSort(x:xs))

pegaMaiores :: Int -> [Int] -> [Int]
pegaMaiores num (x:xs) = pega num (inverteListaOrdenada(x:xs))


--- Implementar a função contaMaiores
--- > contaMaiores 3 [10,2,3,4,5,16]
-- 4
contaMaiores :: Int -> [Int] -> [Int]
contaMaiores 0 (x:xs) = 0
contaMaiores num (x:xs) = somalista (pegaMaiores num xs)