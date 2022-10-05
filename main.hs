-- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.

divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1..n], n `mod` x == 0]

-- 2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada.

contaCaractere :: Char -> String -> Int
contaCaractere c s = length [x | x <- s, x == c]

-- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.

dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo l = [2*x | x <- l, x >= 0]

-- 4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.

pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2 + b^2 == c^2]

-- 5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado.

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1..n], sum (divisoresden x) == x]

-- 6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.

produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar l1 l2 = sum [fst x * snd x | x <- zip l1 l2]

-- 7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2.

primeirosPrimos :: Int -> [Int]
primeirosPrimos n = take n [x | x <- [2..], length (divisoresden x) == 2]

-- 8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes.

paresOrdenados :: Int -> [(Int, Int)]
paresOrdenados n = [(2^x, 3^y) | x <- [0..n], y <- [0..n]]


main = do
  putStr "divisoresden: entrada: 24; resultado: "
  print(divisoresden 24)
  putStr "contaCaractere: entrada: 'a' 'abacate'; resultado: "
  print(contaCaractere 'a' "abacate")
  putStr "dobroNaoNegativo: entrada: [23,-564,234,65,-23,0,1,2,3,4,5,6,7,8,9,10]; resultado: "
  print(dobroNaoNegativo [23,-564,234,65,-23,0,1,2,3,4,5,6,7,8,9,10])
  putStr "pitagoras: entrada: 20; resultado: "
  print(pitagoras 20)
  putStr "numerosPerfeitos: entrada: 7; resultado: "
  print(numerosPerfeitos 7)
  putStr "produtoEscalar: entrada: [1,2,3,4,5] [3,4,5,6,7]; resultado: "
  print(produtoEscalar [1,2,3,4,5] [3,4,5,6,7])
  putStr "primeirosPrimos: entrada: 10; resultado: "
  print(primeirosPrimos 10)
  putStr "paresOrdenados: entrada: 5; resultado: "
  print(paresOrdenados 2)
