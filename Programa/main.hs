{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import qualified Includes.File
import qualified Includes.Date
import System.Exit (exitSuccess)
import Includes.File (printFile)
import qualified Control.Monad
import Includes.Date ( validDate )
import Data.Maybe (isNothing, isJust, fromJust)
import Control.Monad (when, unless)
import Text.Read (readMaybe)
import Data.Time (UniversalTime)

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

-- Obtiene una fecha por input y valida si es correcto el formato
-- Retona un string con la fecha
getDate :: IO String
getDate = do
    -- we define "loop" as a recursive IO action
    let loop = do
            putStrLn "Ingresa la fecha (dd-mm-yyy): "
            putStr ">>"
            entryDateStr <- getLine
            let entryDate = validDate entryDateStr

            if isNothing entryDate then
                do
                    putStrLn "[Error]: El formato de la fecha es incorrecto."
                    loop
            else return entryDateStr
    loop  -- start the first iteration 

-- Obtiene el rango de fecha de reservacion y los valida.
-- Retorna una lista con las fechas de reservacion.
getDateReservation :: IO [String]
getDateReservation = do
    let loop = do
        entryDate <- getDate
        departDate <- getDate

        -- Verifica si la fecha inicial es mayor a la final
        if entryDate > departDate then
            do
                putStrLn "[Error]: La fecha inicial debe ser menor a la fecha final."
                loop
        else return [entryDate,departDate]
    loop

-- Obtiene la cantidad de niños y adultos
-- Retorna una lista con un string de la cantidad de adultos en la posicion 0 y los niños en la 1
getNumAdultChild :: IO [String]
getNumAdultChild = do
    let loopAdult = do
        putStr "\nCantidad de adultos: "
        numAdults <-getLine
        if not (isInt numAdults) then
            do
                putStrLn "[Error]: Lo ingresado no es un número entero."
                loopAdult
        else return numAdults

    let loopChild = do
        putStr "\nCantidad de niños: "
        numChildren <-getLine
        if not (isInt numChildren) then
            do
                putStrLn "[Error]: Lo ingresado no es un número entero."
                loopChild
        else return numChildren
    numadult<-loopAdult
    numChild<-loopChild
    return [numadult,numChild]

-- Obtiene el tipo de habitación, la cantidad de adultos y niños huespedes
-- Retorna una lista de strings con el input de los datos.
getNumAdChRoomType :: IO [String]
getNumAdChRoomType = do
    -- Loop que verifica si el tipo de habitación es válido
    let loopRoom = do
        putStr "\nSeleccione el tipo de habitación: "
        roomType <-getLine
        if not (isInt roomType) then
            do
                putStrLn "[Error]: Lo ingresado no es un número entero."
                loopRoom
        else return roomType

    -- Loop que verifica si es número
    let loopNumAdult = do 
        putStr "\nCantidad de huéspedes adultos: "
        numAdultGuests <-getLine
        if not (isInt numAdultGuests) then
            do
                putStrLn "[Error]: Lo ingresado no es un número entero."
                loopNumAdult
        else return numAdultGuests

    -- Loop que verifica si es número
    let loopNumChild = do
        putStr "\nCantidad de huéspedes niños: "
        numChildGuests <-getLine
        if not (isInt numChildGuests) then
            do
                putStrLn "[Error]: Lo ingresado no es un número entero."
                loopNumChild        
        else return numChildGuests

    roomType <- loopRoom
    numAdult <- loopNumAdult
    numChild <- loopNumChild
    return [roomType,numAdult,numChild]

saveReservation :: Show a => a -> IO ()
saveReservation reservationData= do print reservationData

reservation :: IO ()
reservation = do
    putStrLn "\t\tReservación"

    dateRange <- getDateReservation
    numAdultChild <- getNumAdultChild
    let numAdult = read (head numAdultChild)::Int
    let numChild = read (numAdultChild!!1)::Int

    putStr "Nombre de quien reserva: "
    name <- getLine

    roomTypeData <- getNumAdChRoomType
    let numAdultGuest = read (roomTypeData!!1)::Int
    let numChildGuest = read (roomTypeData!!2)::Int

    -- Verifica si el total de huéspedes suma el total de adultos y niños.
    let numChildAdults = numAdult + numChild
    let numChildAdultsGuests = numAdultGuest + numChildGuest
    when (numChildAdults /= numChildAdultsGuests && numChildAdults > 0) $
        do
            putStrLn "El número de huéspedes y niños no es el mismo que la cantidad de adultos y niños o no son mayores a 0"
            reservation
    let reservationData = dateRange++numAdultChild++[name]++roomTypeData
    saveReservation  reservationData


cancelReservation :: IO ()
cancelReservation = do
    putStrLn "Cancelar reservación"

invoiceReservation :: IO ()
invoiceReservation = do
    putStrLn "Facturar reservación"

menuGeneral :: IO ()
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

