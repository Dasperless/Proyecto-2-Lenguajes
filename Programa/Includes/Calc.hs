module Includes.Calc (
    getRateList,
    getAdultPrice,
    getChildPrice
) where
import Includes.Date(isHighSeason,isWeekend)
import Includes.File(noHeaderData)
import Data.Time

--Retorna el precio por día de adulto
getAdultPrice :: Day -- ^ la fecha de entrada
  -> Day -- ^ la fecha de salida
  -> Int -- ^ El precio a retornar
  -> IO Int
getAdultPrice entryDate departDate price = do
    rateList <-getRateList entryDate
    let rate = head rateList
    if entryDate == departDate
        then  case price of 
                0-> return rate
                _-> return price
    else getAdultPrice (addDays 1 entryDate) departDate (price+rate)

-- Retorna el precio por día de niño
getChildPrice :: Day -- ^ La fecha de entrada
  -> Day -- ^ La fecha de salida
  -> Int -- ^ EL precio que se retornará
  -> IO Int
getChildPrice entryDate departDate price = do
    rateList <-getRateList entryDate
    let rate = last rateList
    if entryDate == departDate
        then  case price of 
                0-> return rate
                _-> return price
    else getAdultPrice (addDays 1 entryDate) departDate (price+rate)

-- Obtiene una lista con los precios de adultos y niños según el día [adulto,niño]
getRateList :: Day -- ^ La fecha
  -> IO [Int]
getRateList date = do
    file <-noHeaderData "./BD/Rates.csv"
    if not(isHighSeason date)
        then do
            if not (isWeekend date)
                then do
                    let adultPrice1 = read (last(head file)) :: Int
                    let childPrice2 = read (last(file!!1)) :: Int
                    return[adultPrice1,childPrice2]
            else do
                let adultPrice3 =  read (last(file!!2)) :: Int
                let childPrice4 =  read (last(file!!3)) :: Int
                return [adultPrice3,childPrice4]
    else
        if not (isWeekend date)
            then
                do
                let adultPrice5 = read (last(file!!4)) :: Int
                let childPrice6 = read (last(file!!5)) :: Int
                return[adultPrice5,childPrice6]
        else do
            let adultPrice7 = read (last(file!!6)) :: Int
            let childPrice8 = read (last(file!!7)) :: Int
            return [adultPrice7,childPrice8]

