module File where
import System.IO

wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

dataMatrixAux :: Monad m => [[Char]] -> [[Char]] -> m [[Char]]
dataMatrixAux list newList =
    if null list
         then return  newList
    else
       dataMatrixAux (init list) (wordsWhen (==',') (last list)++newList)

dataMatrix :: Monad m => [[Char]] -> m [[String]]
dataMatrix list =
    if null list
        then return [[""]]
    else
        return (dataMatrixAux list [])

getLines :: FilePath -> IO [String]
getLines path  = do
    str <- readFile  path
    return (lines str)