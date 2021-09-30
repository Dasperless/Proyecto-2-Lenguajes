module Includes.Calc (getRateList) where
import Includes.Date(isHighSeason,isWeekend)
import Includes.File(noHeaderData)
import Data.Time

getRates date = return()

-- getRateList :: Day -- ^ 
--   -> [String]
-- getRateList :: Day -> IO String
getRateList :: Day -> IO [String]
getRateList date = do
    file <-noHeaderData "./BD/Rates.csv"
    if not(isHighSeason date)
        then do
            if not (isWeekend date)
                then return[last(head file),last(file!!1)]
            else return [last(file!!2),last(file!!3)]
    else
        if not (isWeekend date)
            then return[last(file!!4),last(file!!5)]
        else return [last(file!!6),last(file!!7)]