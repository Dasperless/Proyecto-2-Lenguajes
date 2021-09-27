module Includes.File (
  printFile,
  fileToMatrix,
  getFileLines,
  getFileData,
  writeCsv,
  loadFile,
  noHeaderData,
  getHeaderData,
  addLineCsv
) where
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import Text.Parsec
import qualified Control.Monad
import Data.List (sort,group)




rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

loadFile :: FilePath -- Lee un archivo
  -> IO [Char] --"La dirección de un archivo"
loadFile filename = withFile filename ReadMode $ \handle -> do
  theContent <- hGetContents handle
  mapM return theContent

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
  str <- loadFile path
  return (lines str)

-- Retorna una matriz de los datos del archivo
getFileData :: FilePath -- La ruta del archivo
  -> IO [[[Char]]] -- Matriz de string 
getFileData path = do
  file <- getFileLines path
  return (fileToMatrix file)

-- Retorna el header del archivo csv
getHeaderData :: FilePath -- La ruta del archivo
  -> IO [[Char]] -- Una lista con el header del csv
getHeaderData path = do
  dataFile<-getFileData path 
  return (head dataFile)

-- Retorna los datos del csv sin los encabezados
noHeaderData :: FilePath -- La ruta del archivo
  -> IO [[[Char]]]--Una matriz de string
noHeaderData path =  do 
    dataFile<-getFileData path 
    return (tail dataFile)

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
addLineCsv :: FilePath -> [[Char]] -> IO ()
addLineCsv path newData = do
  let dataFileStr = listToString newData
  appendFile path dataFileStr

matrixToString :: [[[Char]]] -> [Char]
matrixToString matrix = do
    if null matrix then
      ""
    else listToString (head matrix) ++ matrixToString (tail matrix)

-- Elimina todo el contenido de un archivo y agrega nueva información
-- path = dirección del archivo
-- newData = lista de strings a escribir en el archivo
writeCsv :: FilePath -> [[[Char]]] -> IO ()
writeCsv path newData = do
  let newDataStr = matrixToString newData
  writeFile path newDataStr