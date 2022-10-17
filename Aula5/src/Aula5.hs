{-|
Module : Aula5
Description : Módulo Haskell contendo a resolução das tarefas propostas na Ficha 5 de LI1.
Copyright : Rafael Fernandes <a104271@alunos.uminho.pt>;

Este módulo contém definições Haskell para as tarefas proposta na Ficha 5 de LI1.
-}
module Aula5 where

{- | A função 'areaQuad' calcula a área de um quadrado dada a medida do seu lado.

 == Exemplos de Utilização
 
 >>> areaQuad 5
 25
 -}
areaQuad :: Float -> Float
areaQuad l = l ^ 2

{- | A função 'allBut', recebendo uma lista de Strings, retorna todos os elementos que não comecem por um caractere.

 == Exemplos de Utilização
 
 >>> allBut ["Foo", "Bar", "Baz"] 'B'
 ["Foo"]

 >>> allBut ["Foo", "Bar", "Baz"] 'F'
 ["Bar", "Baz"]
 -}
allBut:: [String] -> Char -> [String]
allBut l c = filter (\x -> head x /= c) l

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