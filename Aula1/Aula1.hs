{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-|
Module : Aula1
Description : Módulo Haskell contendo a resolução das tarefas propostas na Ficha 1 de LI1.
Copyright : Rafael Fernandes <a104271@alunos.uminho.pt>;

Este módulo contém definições Haskell para as tarefas proposta na Ficha 1 de LI1.
-}
module Aula1 where

-- a
{- | A função 'areaQuad' calcula a área de um quadrado dada a medida do seu lado.

 == Exemplos de Utilização
 
 >>> areaQuad 5
 25
 -}
areaQuad :: Float -> Float
areaQuad l = l ^ 2

-- b
{- | A função 'perimetroRect' calcula a o perímetro de um retângulo dadas as medida da sua largura e comprimento.

 == Exemplos de Utilização
 
 >>> perimetroRect 5 6
 26
 -}
perimetroRect :: Float -> Float -> Float
perimetroRect l a = l * 2 + a * 2

-- c
{- | A função 'hasChar' verifica se um caractere existe na String fornecida.

 == Exemplos de Utilização
 
 >>> hasChar 'a' "abc"
 True

 >>> hasChar 'd' "abc"
 False
 -}
hasChar c s = c `elem` s

-- d
{- | A função 'trimmer' retorna todos os elementos de uma lista fornecida exceto o último caso o tamanho da lista seja ímpar, ou todos os
elementos de uma lista fornecida exceto o primeiro caso o tamanho da lista seja par.

 == Exemplos de Utilização
 
 >>> trimmer [1,2,3]
 [1,2]

 >>> trimmer [1,2,3,4]
 [2,3,4]

 == Propriedades
 prop> length l mod` 2 == 0 => tail l
 prop> length l mod` 2 /= 0 => init l
 -}
trimmer:: [a] -> [a]
trimmer l | length l `mod` 2 == 0 = tail l
          | otherwise = init l

-- e
{- | A função 'firstLast' retorna o primeiro e último elementos de uma lista.

 == Exemplos de Utilização
 
 >>> firstLast [1,2,3]
 (1,3)
 -}
firstLast :: [a] -> (a, a)
firstLast (h:t) = (h, last t)

-- f
{- | A função 'primeiroUltimoNome' retorna o primeiro e último nome de uma lista contendo o nome de uma pessoa.

 == Exemplos de Utilização
 
 >>> primeiroUltimoNome ["Rafael", "Santos", "Fernandes"]
 ("Rafael", "Fernandes")
 -}
primeiroUltimoNome :: [a] -> (a, a)
primeiroUltimoNome n = (head n, last n)

-- g
{- | A função 'firstHeadSecondList', recebendo um tuplo de uma lista e um outro elemento de qualquer tipo, retorna um tuplo
contendo o primeiro elemento da lista e o outro elemento fornecido inalterado.

 == Exemplos de Utilização
 
 >>> firstHeadSecondList ([1,2,3], 4)
 (1,4)
 -}
firstHeadSecondList :: ([a], b) -> (a, b)
firstHeadSecondList (xs, ys) = (head xs, ys)

-- h
{- | A função 'nomeFormatado', recebendo uma lista contendo o nome de uma pessoa, retorna uma String contendo a inicial
do primeiro nome da pessoa seguido de um ponto ('.') seguido do último nome da pessoa.

 == Exemplos de Utilização
 
 >>> nomeFormatado ["Rafael", "Santos", "Fernandes"]
 "R.Fernandes"
 -}
nomeFormatado :: [String] -> String
nomeFormatado n = [head (head n), '.'] ++ last n