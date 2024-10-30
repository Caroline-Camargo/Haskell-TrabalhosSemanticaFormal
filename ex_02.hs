--Implementar uma função que verifica se uma string contém um palindromo
palindromo :: String -> Bool
palindromo str = str == (reverse str)

--A soma do comprimento de qualquer dois lados de um triângulo é sempre maior do que o comprimento do terceiro lado. Fazer uma função que recebe o comprimento dos três lados de um triângulo e verifica essa condição
comprimentoLadoTriangulo :: Int -> Int -> Int -> Bool
comprimentoLadoTriangulo a b c = ((a + b) >= c) && ((a + c) >= b) && ((b + c) >= a)

--Defina, usando guardas, a função sinal que recebe um inteiro como entrada e devolve: -1 se a entrada for um número negativo, 1, caso seja positivo ou 0 caso a entrada seja o número zero
sinal :: Int -> Int
sinal x
  | x == 0 = 0
  | x > 0 = 1
  | otherwise = -1
  
--Implemente a função: que recebe três inteiros e devolve o menor entre os três
menorTres :: Int -> Int -> Int -> Int
menorTres a b c
   | (a < b) && (a < c) = a
   | (b < a) && (b < c) = b
   | otherwise = c
  
--Em Haskell, para implementarmos algum tipo de repetição, usamos recursão. Por exemplo, sabemos que o fatorial de um número n é calculado a partir da multiplicação desse número com todos os seus atencessores até o número 1. Por exemplo, o fatorial de 4 é calculado da seguinte forma:
--4 * 3 * 2 * 1
--Em Haskell, podemos implementar uma função que calcula o fatorial de um número da seguinte forma:
fat :: Int -> Int
fat 0 = 1
fat 1 = 1
fat n = n * fat (n-1)

--A primeira equação diz que o fatorial de 0 é 1. A segunda diz que o fatorial do número 1 é 1. A terceira equação diz que se a função fat receber um número n (diferente de 1 e 0), calculamos n * fat (n-1). Exercício: Implementar uma função recursiva que recebe a base e o expoente e calcula a potência:
potencia :: Int -> Int -> Int
potencia a 0 = 1
potencia a 1 = a
potencia a b = a * potencia a (b-1)

--Escreva a função osQuatroSaoIguais que possui tipo Int -> Int -> Int -> Int -> Bool que retorna True se seus quatro argumentos s˜ao iguais
testeIgual :: Int -> Int -> Int -> Int -> Bool
testeIgual a b c d = (a==b) && (b==c) && (c==d)
 
--Escreva a função osQuatroSaoIguais que possui tipo Int -> Int -> Int -> Int -> Bool que retorna True se seus quatro argumentos s˜ao iguais
testeIgual :: Int -> Int -> Int -> Int -> Bool
testeIgual a b c d = (a==b) && (b==c) && (c==d)

--Defina a função quantosSaoIguais :: Int -> Int -> Int -> Int que recebe 3 valores e diz quantos desses valores s˜ao iguais
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
  | (a==b) && (b==c) = 3
  | (a==b) || (b==c) || (a==c) = 2
  | otherwise = 1

--O que est´a errado com a seguinte defini¸c˜ao de todosDiferentes todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
todosDiferentes:: Int -> Int -> Int -> Int
todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
