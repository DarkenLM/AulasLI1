{-|
Module : Aula3
Description : Módulo Haskell contendo a resolução das tarefas propostas na Ficha 4 de LI1.
Copyright : Rafael Fernandes <a104271@alunos.uminho.pt>;

Este módulo contém definições Haskell para as tarefas proposta na Ficha 4 de LI1.
-}
module Aula3 where

{- | A função 'shiftRightN' desloca todos os elementos da lista @n@ posições para a direita. Os elementos deslocados para
uma posição superior ao tamanho da lista serão colocados no inicio da lista.

 == Exemplos de Utilização
 
 >>> shiftRightN [1,2,3,4,5] 2
 [4,5,1,2,3]
 -}
shiftRightN :: [Int] -> Int -> [Int]
shiftRightN [] _ = []
shiftRightN l n = let (elems, nl) = sRN [] l n in elems ++ nl
    where
        sRN acc l 0 = (reverse acc, l)
        sRN acc l n = sRN (acc ++ [last l]) (init l) (n-1)

{- | A função 'shiftLeftN' desloca todos os elementos da lista @n@ posições para a esquerda. Os elementos deslocados para
uma posição inferior ao tamanho da lista serão colocados no fim da lista.

 == Exemplos de Utilização
 
 >>> shiftLeftN [1,2,3,4,5] 2
 [3,4,5,1,2]
 -}
shiftLeftN [] _ = []
shiftLeftN l 0 = l
shiftLeftN (h:t) n = shiftLeftN (t ++ [h]) (n-1)

{- | Representa uma Matriz matemática

 == Observações
 Assume-se que todas as linhas têm o mesmo número de elementos
 -}
type Matriz = [[Int]]

{- | A função 'swapLines' troca as posições da primeira e última linha de uma matriz.

 == Exemplos de Utilização
 
 >>> swapLines [[1,2],[3,4],[5,6]]
 [[5,6],[3,4],[1,2]]
 -}
swapLines :: Matriz -> Matriz
swapLines [] = []
swapLines [x] = [x]
swapLines (h:t) = last t : init t ++ [h]

{- | A função 'swapColumns' troca as posições da primeira e última coluna de uma matriz.

 == Exemplos de Utilização
 
 >>> swapColumns [[1,2],[3,4],[5,6]]
 [[2,1],[4,3],[6,5]]
 -}
swapColumns :: Matriz -> Matriz
swapColumns [] = []
swapColumns (h:t) = swapper h : swapColumns t --map swapper m
    where
        swapper [] = []
        swapper (h:t) = last t : init t ++ [h]

{- | A função 'elemIndex' retorna a posição da primeira ocorrência de um elemento, caso exista. Caso contrário retorna -1.

 == Exemplos de Utilização
 
 >>> elemIndex [1,2,3] 3
 2

 >>> elemIndex [1,2,3] 4
 -1
 -}
elemIndex :: (Eq t) => [t] -> t -> Int
elemIndex [] _ = -1
elemIndex l e = indexer 0 l e
    where
        indexer _ [] _ = -1
        indexer acc (h:t) e | e == h = acc
                            | otherwise = indexer (acc+1) t e

{- | A função 'replaceAt' substitui o elemento localizado na posição @n@ de uma lista por um elemento especificado.

 == Exemplos de Utilização
 
 >>> replaceAt "abcdefg" 3 'X'
 "abcXefg"
 -}
replaceAt :: [a] -> Int -> a -> [a]
replaceAt l p e = replacer 0 l p e
    where
        replacer :: Int -> [a] -> Int -> a -> [a]
        replacer _ [] _ _ = []
        replacer acc l p e | p < 0 = error "Posição não pode ser negativa."
                           | acc == p = take acc l ++ [e] ++ reverse (take (length l - 1 - acc) (reverse l))
                           | p > length l - 1 = error "Posição excede o tamanho da lista."
                           | otherwise = replacer (acc+1) l p e

{- | A função 'searchMatrix' retorna a posição da primeira ocorrência de um elemento especificado numa matriz, caso exista. 
Caso contrário retorna -1.

 == Exemplos de Utilização
 
 >>> searchMatrix [[1,0],[2,3]] 3
 (1,1)

 >>> searchMatrix [[1,0],[2,3]] 4
 (-1,-1)
 -}
searchMatrix :: Matriz -> Int -> (Int,Int)
searchMatrix m e = searcher 0 m e
    where
        searcher :: Int -> Matriz -> Int -> (Int,Int)
        searcher _ [] _ = (-1,-1)
        searcher lin m e | lin > length m = (-1,-1)
                         | (lin < length m) && (elemIndex (m !! lin) e > -1) = (lin,elemIndex (m !! lin) e)
                         | otherwise = searcher (lin+1) m e

{- | A função 'replaceMatrixAt' substitui o elemento localizado na posição @(l,c)@ de uma matriz por um elemento especificado.

 == Exemplos de Utilização
 
 >>> replaceMatrixAt [[1,0],[3,4]] (0,1) 2
 [[1,2],[3,4]]
 -}
replaceMatrixAt :: Matriz -> (Int,Int) -> Int -> Matriz
replaceMatrixAt [] _ _ = []
replaceMatrixAt m (lin,col) e | (lin > length m) || (col > length (head m)) = error "Posição inválida"
                              | otherwise = take lin m ++ [replaceAt (m !! lin) col e] ++ reverse (take (length m - 1 - lin) (reverse m))