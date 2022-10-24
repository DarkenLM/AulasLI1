{-|
Module : MiniTeste
Description : Módulo contendo as definições das funções exigidas no enunciado do Miniteste de LI1.
Copyright : Rafael Fernandes <a104271@alunos.uminho.pt>

O Módulo MIniTeste contém as definições para as funções exigidas no enunciado do Miniteste realizado no dia 24 de Outubro 
de 2022, no âmbito da disciplina de Laboratórios de Informática I, incluída no primeiro ano do Curso de Licenciatura de 
Engenharia Informática da Universidade do Minho.
-}
module MiniTeste where


{- |
  A função f, dada uma lista de pares de inteiros, subtrai uma unidade da segunda componente de cada par existente na lista.
Caso o valor da segunda componente da lista após a sua tranformação for igual ao valor 0, o par é removido da lista final.

  == Exemplos de utilização:

  >>> f [(2,1),(1,5),(4,6)]
  [(1,4),(4,5)]

  >>> f [(1,-2),(3,4),(5,0)]
  [(1,-3),(3,3),(5,-1)]
-}
f :: [(Int, Int)] -> [(Int, Int)]
f [] = []
f ((h1,h2):t) 
    | h2 - 1 == 0 = f t
    | otherwise = (h1, h2 - 1) : f t