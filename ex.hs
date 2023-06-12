module Main where
main = do putStrLn "Hello world!"


incrementa::[Int]->Int->[Int]
incrementa [] e = []
incrementa (x:xs) e = (x+e):(incrementa xs e)

remo:: [String]->Char->[String]
remo [] c = []
remo (x:xs) c = if (head x) == c then remo xs c
				else x:(remo xs c)




module Exercicios_Spec where main

import  Test.HUnit
{- esta funçao...-}
dobro:: [Int] -> [Int]
dobro [] = []
dobro (x:xs) = 2 * x : dobro xs

testDobroListaVazia = [] ~=? dobro []
testDobroLista12 = [2,4] ~=? dobro [1,2]

testsDobro = TestList [
	testDobroListaVazia,
	testDobroLista12]
