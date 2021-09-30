module Includes.Date
  ( parseDate,
    isHighSeason,
    getDateTuple,
    isWeekend
  )
where

import Data.Time (UniversalTime, defaultTimeLocale, parseTimeM, Day, toGregorian, dayOfWeek, DayOfWeek (Friday,Saturday,Sunday))

-- Convierte un string en una fecha 
parseDate :: String -- ^ String con la fecha
  -> Maybe Day --Retorna maybe si la fecha es valida, Nothing en caso contrario
parseDate date = do
  parseTimeM True defaultTimeLocale "%d/%m/%Y" date :: Maybe Day

-- Obtiene una tupla con los datos de la fecha
getDateTuple :: Day -- ^ La fecha 
  -> IO(Integer, Int, Int)
getDateTuple date = return (toGregorian date)

-- Verifica si el día de una fecha es fin de semana
isWeekend :: Day -- ^ La fecha 
  ->Bool --True si es fin de semana, False en caso contrario
isWeekend date = do
    let dayWeek = dayOfWeek date
    case dayWeek of
      Friday -> True
      Saturday ->True
      _ ->False

-- Verifica si la fecha es temporada alta o baja
isHighSeason :: Day -- ^ La fecha
  ->Bool--True si es temporada alta, False en caso contrario
isHighSeason date = do
  let (year,month,day)=toGregorian date
  not (month>=4 && month <=10)

-- isHighSeason entryDate departDate = do 
--   if 
