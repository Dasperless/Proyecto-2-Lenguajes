module Includes.File (
  printFile,
  fileToMatrix,
  getFileLines,
  writeCsv
) where
import System.IO ()
import qualified Data.ByteString.Char8 as B

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
fileToMatrixAux :: [[Char]] -> [[[Char]]] -> [[[Char]]]
fileToMatrixAux list newList = do
  if null list
    then newList
    else fileToMatrixAux (init list) newList ++ [wordsWhen (== ',') (last list)]

-- Convierte una lista de strings y las convierte en una matriz
-- Recibe :
--  list: Una lista de tipo [[char]]
-- Retorna:
--  Una matris de tipo [[[char]]]
fileToMatrix :: [[Char]] -> [[[Char]]]
fileToMatrix list = do
  if null list
    then [[]]
    else fileToMatrixAux list []

-- Crea una lista de las lineas de un archivo
-- path: un string con la dirección del archivo
getFileLines :: FilePath -> IO [String]
getFileLines path = do
  str <- readFile path
  return (lines str)

-- Imprime una lista 
-- Recibe una lista de strings
printRow :: [[Char]] -> IO ()
printRow row = do
  if null row
    then putStr "\n"
    else
      do
      putStr $ head row ++ "\t"
      printRow (tail row)

-- Imprime una matriz
-- Recibe una matriz de strings
printMatrix :: [[[Char]]] -> IO ()
printMatrix matrix = 
  if null matrix
    then putStr ""
  else
    do
    printRow (head matrix)
    printMatrix (tail matrix)

-- Imprime un archivo
-- Recibe:
--  path: la dirección de un archivo
printFile :: FilePath -> IO ()
printFile path = do
  file <- getFileLines path
  let dataFile = fileToMatrix file
  printMatrix dataFile

-- Convierte una lista a un string
-- Recibe:
--  list: lista de string
-- Retorna: string
listToString :: [[Char]] -> [Char]
listToString list 
  | null list = return '\n'
  | length list == 1 = head list++listToString (tail list)
  | otherwise = head list++","++listToString (tail list)
 
-- Añada una nueva linea a un archivo csv
-- Recibe:
--  path: la dirección del archivo
--  newData: Una lista de strings a escribir en un csv
writeCsv :: FilePath -> [[Char]] -> IO ()
writeCsv path newData = do
  let dataFileStr = listToString newData
  appendFile path dataFileStr