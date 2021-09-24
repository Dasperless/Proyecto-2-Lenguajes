{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import qualified Includes.File
import qualified Includes.Date
import System.Exit (exitSuccess)
import Includes.File (printFile)
import qualified Control.Monad
import Includes.Date
import Data.Maybe (isNothing, isJust)
import Control.Monad (when, unless)
import Text.Read (readMaybe)

isInt :: String -> Bool
isInt num = do
    let x = readMaybe num :: Maybe Int
    isJust x

hotelInfo :: IO ()
hotelInfo = do
    printFile "./BD/info.csv"

loadRoomType :: IO ()
loadRoomType = do
    putStrLn "Cargar tipo de habitaciones"

numRoomsByType = do
    putStrLn "Asignar Cantidad de habitaciones por tipo"

chargeRates = do
    putStrLn "Carga de Tarifas"

consultReservations = do
    putStrLn "Consultar Reservaciones"

consultInvoice = do
    putStrLn "Consulta de facturas"

occupancyStatistics = do
    putStrLn "Estadísticas de ocupación"

menuAdmin :: IO ()
menuAdmin = do
    putStrLn "\t\tOpciones Administrativas"
    putStrLn "1- Información de hotel"
    putStrLn "2- Cargar tipo de habitaciones"
    putStrLn "3- Asignar Cantidad de habitaciones por tipo"
    putStrLn "4- Carga de Tarifas"
    putStrLn "5- Consultar Reservaciones"
    putStrLn "6- Consulta de facturas"
    putStrLn "7- Estadísticas de ocupación"
    putStrLn "8- Salir"
    putStr ">>"
    option <- getLine
    case option of  "1" -> hotelInfo
                    "2" -> loadRoomType
                    "3" -> numRoomsByType
                    "4" -> chargeRates
                    "5" -> consultReservations
                    "6" -> consultInvoice
                    "7" -> occupancyStatistics
                    "8" -> return()
                    _ -> putStrLn "[ERROR]: La opción elegida no es válida"

    when (option /= "8") menuAdmin

reservation = do
    putStrLn "\t\tReservación"

    -- FECHA DE INGRESO
    putStrLn "Fecha de ingreso [Ejemplo]: 23-09-2021"
    putStr ">>"
    entryDateStr <- getLine
    let entryDate = validDate entryDateStr

    when (isNothing entryDate) $
        do
            putStrLn "[Error]: El formato de la fecha es incorrecto."
            reservation

    -- FECHA DE SALIDA
    putStrLn "\nFecha de salida [Ejemplo]: 24-09-2021 "
    putStr ">>"
    departDateStr <- getLine
    let departDate = validDate departDateStr

    when (isNothing departDate) $
        do
            putStrLn "[Error]: El formato de la fecha es incorrecto."
            reservation

    -- Verifica si la fecha inicial es mayor a la final
    when (entryDate > entryDate) $
        do
            putStrLn "[Error]: La fecha inicial debe ser menor a la fecha final."
            reservation

    putStr "\nCantidad de adultos: "
    numAdults <-getLine
    unless (isInt numAdults) $
        do
            putStrLn "[Error]: Lo ingresado no es un númeor entero."
            reservation

    putStr "\nCantidad de niños: "
    numChildren <-getLine
    unless (isInt numChildren) $
        do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            reservation


    putStr "\nSeleccione el tipo de habitación: "
    roomType <-getLine
    unless (isInt roomType) $
        do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            reservation

    putStr "\nCantidad de huéspedes adultos: "
    numAdultGuests <-getLine
    unless (isInt numAdultGuests) $
        do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            reservation

    putStr "\nCantidad de huéspedes niños: "
    numChildGuests <-getLine
    unless (isInt numChildGuests) $
        do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            reservation




cancelReservation = do
    putStrLn "Cancelar reservación"

invoiceReservation :: IO ()
invoiceReservation = do
    putStrLn "Facturar reservación"

menuGeneral = do
    putStrLn "\t\tOpciones Generales"
    putStrLn "1- Reservación"
    putStrLn "2- Cancelar reservación"
    putStrLn "3- Facturar reservación"
    putStrLn "4- Salir"
    putStr ">>"
    option <- getLine
    case option of  "1" -> reservation
                    "2" -> cancelReservation
                    "3" -> invoiceReservation
                    "4" -> return ()
                    _ -> putStrLn "[ERROR]: La opción elegida no es válida"
    Control.Monad.when (option /= "4") menuGeneral

