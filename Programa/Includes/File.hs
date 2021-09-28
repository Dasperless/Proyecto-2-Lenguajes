module Includes.File (
  printFile,
  fileToMatrix,
  getFileLines,
  getFileData,
  writeCsv,
  loadFile,
  noHeaderData,
  getHeaderData,
  roomTypeRef,
  readRoomTypePath,
  newRoomTypePath,
  addLineCsv
) where
import System.IO (withFile, IOMode (ReadMode), hGetContents)
import System.IO.Unsafe (unsafePerformIO)
import Text.Parsec
import qualified Control.Monad
import Data.List (sort,group)
import Data.IORef

-- Representación de variable global del
-- tipo de cuarto.
roomTypeRef :: IORef String
roomTypeRef =
  unsafePerformIO (newIORef "./BD/RoomType.csv")
{-# NOINLINE roomTypeRef #-}

-- Lee la variable global del tipo de cuarto
readRoomTypePath :: IO String
readRoomTypePath =
  readIORef roomTypeRef

-- Modifica el path del tipo de cuarto
newRoomTypePath ::
  String -> -- ^ Recibe un string con la ruta
  IO ()
newRoomTypePath path =
  atomicModifyIORef' roomTypeRef (const (path, ()))

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Carga una lista con los datos del archivo
loadFile :: FilePath -- ^  La dirección de un archivo
  -> IO [Char] 
loadFile filename = withFile filename ReadMode $ \handle -> do
  theContent <- hGetContents handle
  mapM return theContent

-- Separa un string si es igual a un token
wordsWhen :: (Char -> Bool) -- ^ Un booleano ej: (==',')
  -> [Char] -- ^ El caracter delimitador
  -> [[Char]]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

-- Función auxiliar que crea una matriz separando de los strings por coma
fileToMatrixAux :: [[Char]] -- ^ Una lista de strings
  -> [[[Char]]] -- ^ Lista vacía
  -> [[[Char]]] 
fileToMatrixAux list newList = do
  if null list
    then newList
    else fileToMatrixAux (init list) newList ++ [wordsWhen (== ',') (last list)]

-- Convierte una lista de strings y las convierte en una matriz
fileToMatrix :: [[Char]] -- ^ Una lista de tipo [[char]]
  -> [[[Char]]] -- Una matriz de tipo [[[char]]]
fileToMatrix list = do
  if null list
    then [[]]
    else fileToMatrixAux list []

-- Crea una lista de las lineas de un archivo
getFileLines :: FilePath -- ^La dirección del archivo
  -> IO [String] --Lista de las lineas del archivo
getFileLines path = do
  str <- loadFile path
  return (lines str)

-- Retorna una matriz de los datos del archivo
getFileData :: FilePath -- ^ La ruta del archivo
  -> IO [[[Char]]] -- Matriz de string 
getFileData path = do
  file <- getFileLines path
  return (fileToMatrix file)

-- Retorna el header del archivo csv
getHeaderData :: FilePath -- ^ La ruta del archivo
  -> IO [[Char]] 
getHeaderData path = do
  dataFile<-getFileData path 
  return (head dataFile)

-- Retorna los datos del csv sin los encabezados
noHeaderData :: FilePath -- ^ La ruta del archivo
  -> IO [[[Char]]]--Una matriz de string
noHeaderData path =  do 
    dataFile<-getFileData path 
    return (tail dataFile)

-- Imprime una lista 
printRow :: [[Char]] -- Una lista de strings
  -> IO () -- Printea la lista
printRow row = do
  if null row
    then putStr "\n"
    else
      do
      putStr $ head row ++ "\t"
      printRow (tail row)

-- Imprime una matriz
printMatrix :: [[[Char]]] -- ^ Recibe una matriz de strings
  -> IO ()
printMatrix matrix =
  if null matrix
    then putStr ""
  else
    do
    printRow (head matrix)
    printMatrix (tail matrix)

-- Imprime un archivo
printFile :: FilePath -- ^ la dirección de un archivo
  -> IO ()
printFile path = do
  file <- getFileLines path
  let dataFile = fileToMatrix file
  printMatrix dataFile

-- Convierte una lista a un string
listToString :: [[Char]] -- ^ lista de string
  -> [Char]
listToString list
  | null list = return '\n'
  | length list == 1 = head list++listToString (tail list)
  | otherwise = head list++","++listToString (tail list)

-- Añada una nueva linea a un archivo csv
addLineCsv :: FilePath -- ^ la dirección del archivo
  -> [[Char]] -- ^ Una lista de strings a escribir en un csv
  -> IO ()
addLineCsv path newData = do
  let dataFileStr = listToString newData
  appendFile path dataFileStr

-- Convierte una matriz a string
matrixToString :: [[[Char]]] -- Una matriz de strings 
  -> [Char]
matrixToString matrix = do
    if null matrix then
      ""
    else listToString (head matrix) ++ matrixToString (tail matrix)

-- Elimina todo el contenido de un archivo y agrega nueva información
writeCsv :: FilePath -- ^ La ruta del archivo
  -> [[[Char]]] -- ^ lista de strings a escribir en el archivo
  -> IO ()
writeCsv path newData = do
  let newDataStr = matrixToString newData
  writeFile path newDataStr