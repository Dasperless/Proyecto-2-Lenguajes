module Includes.Date
  ( validDate,
  )
where

import Data.Time (UniversalTime, defaultTimeLocale, parseTimeM)

validDate :: String -> Maybe UniversalTime
validDate date = do
  parseTimeM False defaultTimeLocale "%d-%m-%Y" date :: Maybe UniversalTime