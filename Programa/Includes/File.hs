module File where

import System.IO

-- Separa un string si es igual a un token
-- Ejemplo:
-- >> wordsWhen (==',') "hola,mundo" -> ["hola","mundo"]
wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- Función auxiliar que crea una matriz separando de los strings por coma
-- Recibe:
--  list: una lista de strings
--  newList: una lista vacía
-- Retorna:
--  Una matriz de strings
dataMatrixAux :: [[Char]] -> [[[Char]]] -> [[[Char]]]
dataMatrixAux list newList = do
  if null list
    then newList
    else dataMatrixAux (tail list) newList ++ [wordsWhen (== ',') (head list)]

-- Convierte una lista de strings y las convierte en una matriz
-- Recibe :
--  list: Una lista de tipo [[char]]
-- Retorna:
--  Una matris de tipo [[[char]]]
dataMatrix :: [[Char]] -> [[[Char]]]
dataMatrix list = do
  if null list
    then [[]]
    else dataMatrixAux list []

-- Crea una lista de las lineas de un archivo
-- path: un string con la dirección del archivo
getLines :: FilePath -> IO [String]
getLines path = do
  str <- readFile path
  return (lines str)
