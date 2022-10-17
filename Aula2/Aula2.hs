{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-|
Module : Aula2
Description : Módulo Haskell contendo a resolução das tarefas propostas na Ficha 2 de LI1.
Copyright : Rafael Fernandes <a104271@alunos.uminho.pt>;

Este módulo contém definições Haskell para as tarefas proposta na Ficha 2 de LI1.
-}
module Aula2 where
import Data.List (elemIndex, sortBy)

-- 1
{- | A função 'addToList', recebendo uma lista de inteiros, adiciona um inteiro a cada elemento da lista.

 == Exemplos de Utilização
 
 >>> addToList [1,2,3] 1
 [2,3,4]
 -}
addToList:: [Int] -> Int -> [Int]
addToList l i | null l = l
              | otherwise = (head l + i) : addToList (tail l) i--map(+i) l

-- 2
{- | A função 'allBut', recebendo uma lista de Strings, retorna todos os elementos que não comecem por um caractere.

 == Exemplos de Utilização
 
 >>> allBut ["Foo", "Bar", "Baz"] 'B'
 ["Foo"]

 >>> allBut ["Foo", "Bar", "Baz"] 'F'
 ["Bar", "Baz"]
 -}
allBut:: [String] -> Char -> [String]
allBut l c = filter (\x -> head x /= c) l

-- 3
{- | A função 'addToEveryFirst', recebendo uma lista contendo tuplos de dois inteiros, adiciona um outro inteiro a todos
os primeiros elementos de cada tuplo contido na lista.

 == Exemplos de Utilização
 
 >>> addToEveryFirst [(1,2),(3,4),(5,6)] 1
 [(2,2),(4,4),(6,6)]
 -}
addToEveryFirst:: [(Int, Int)] -> Int -> [(Int, Int)]
addToEveryFirst l i = map (\t -> let (f, s) = t in (f+i, s)) l

--4
{- | A função 'maxSecond', recebendo uma lista contendo tuplos de dois inteiros, retorna o maior valor entre todos
os segundos elementos de cada tuplo.

 == Exemplos de Utilização
 
 >>> maxSecond [(1,2),(3,4)]
 4
 -}
maxSecond:: [(Int, Int)] -> Int
maxSecond l | null l = 0
            | otherwise = let ((h1, h2):t) = l in max h2 (if not (null t) then maxSecond t else h2)
-- maxSecond ((h1, h2):t) = do
--     print h1
--     print h2
--     print t
--     maxSecond t

-- 5
{- | A função 'getNext', recebendo um inteiro contido entre 0 e 9, retorna o número precedente desse inteiro.
Considera-se o zero (0) como o precedente do nove (9).

 == Exemplos de Utilização
 
 >>> getNext 1
 2
 -}
getNext:: Int -> Int
getNext i = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0] !! (i `mod` 10)

-- 6
{- | A função 'advanceAllDigits', recebendo uma lista de inteiros contidos entre 0 e 9, retorna o número precedente de cada 
elemento.

Considera-se o zero (0) como o precedente do nove (9).

 == Exemplos de Utilização
 
 >>> advanceAllDigits [1,2,3]
 [2,3,4]
 -}
advanceAllDigits:: [Int] -> [Int]
advanceAllDigits = map getNext

-- 7
{- | A função 'advanceAllVowels', recebendo uma lista de vogais, retorna a vogal precedente de cada elemento.

Considera-se o "a" como o precedente do "u".

 == Exemplos de Utilização
 
 >>> advanceAllVowels ['a','e','i','o','u']
 ['e','i','o','u','a']
 -}
advanceAllVowels:: [Char] -> [Char]
--advanceAllVogals l = map (\x -> ["e", "i", "o", "u", "a"] !! ((elemIndex x ["a", "e", "i", "o", "u"]) `mod` 5)) l
advanceAllVowels = map (\x ->
    let ind = elemIndex x ['a', 'e', 'i', 'o', 'u']
        offsetVowels = ['e', 'i', 'o', 'u', 'a']
    in case ind of
        Just i -> offsetVowels !! (i `mod` 5)
        Nothing -> error "Character is not a vogal"
    )

--8
type Nome = String
type Coordenada = (Int, Int)
data Movimento= N | S | E | W deriving (Show,Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

-- 8a
{- | A função 'posicao', recebendo uma 'PosicaoPessoa' e uma lista de 'Movimentos', retorna a nova posição após executada a
lista de movimentos.

 == Exemplos de Utilização
 
 >>> posicao (Pos "Foo" (0,0)) [N,N]
 Pos "Foo" (0,2)

 >>> posicao (Pos "Foo" (0,-2)) [S,S]
 Pos "Foo" (0,2)

 >>> posicao (Pos "Foo" (-2,0)) [W,W]
 Pos "Foo" (0,2)

 >>> posicao (Pos "Foo" (2,0)) [E,E]
 Pos "Foo" (0,2)
 -}
posicao:: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao (Pos n (x, y)) mov | null mov = Pos n (x, y)
                           | otherwise = let (m:om) = mov in case () of
                            () | m == N -> let pp = Pos n (x, y+1) in posicao pp om
                               | m == S -> let pp = Pos n (x, y-1) in posicao pp om
                               | m == E -> let pp = Pos n (x+1, y) in posicao pp om
                               | m == W -> let pp = Pos n (x-1, y) in posicao pp om

-- 8b
{- | A função 'posicoesM', recebendo uma lista de 'PosicaoPessoa's e um 'Movimento', retorna uma lista contendo as novas 
posições após executado o movimento em cada uma delas.

 == Exemplos de Utilização
 
 >>> posicoesM [(Pos "Foo" (0,0)),(Pos "Bar" (1,1))] N
 [Pos "Foo" (0,1),Pos "Bar" (1,2)]
 -}
posicoesM:: [PosicaoPessoa] -> Movimento -> [PosicaoPessoa]
posicoesM l mov = map (\x -> posicao x [mov]) l

-- 8c
{- | A função 'posicoesMs', recebendo uma lista de 'PosicaoPessoa's e uma lista de 'Movimento's, retorna uma lista contendo as novas 
posições após executada a lista de movimentos em cada uma delas.

 == Exemplos de Utilização
 
 >>> posicoesMs [(Pos "Foo" (0,0)),(Pos "Bar" (1,1))] [N,W]
 [Pos "Foo" (-1,1),Pos "Bar" (0,2)]
 -}
posicoesMs:: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicoesMs l mov = map (\x -> posicao x mov) l

-- 8d
{- | A função 'pessoasNorte', recebendo uma lista de 'PosicaoPessoa's, retorna uma lista contendo os nomes das posições 
contendo a maior coordenada y.

 == Exemplos de Utilização
 
 >>> pessoasNorte [(Pos "Foo" (0,0)),(Pos "Bar" (1,1))]
 ["Bar"]

 >>> pessoasNorte [(Pos "Foo" (100,1)),(Pos "Bar" (1,1))]
 ["Foo","Bar"]
 -}
pessoasNorte:: [PosicaoPessoa] -> [Nome]
pessoasNorte p = do
    let sortedP = sortBy (\(Pos _ (_, y1)) (Pos _ (_, y2)) -> compare y2 y1) p
        Pos _ (_, maxY) = head sortedP
        northest = maxY
    map (\(Pos n _) -> n) (filter (\(Pos _ (_, y)) -> y == northest) sortedP)