import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (UniversalTime)
import Includes.Date (validDate)
import Includes.File (addLineCsv, getFileData, noHeaderData,getHeaderData, printFile, writeCsv)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Verifica si un número es entero
isInt ::
  String -> -- Obtiene un string
  Bool --Retorna True si representa un entero, False en caso contrario
isInt num = do
  let x = readMaybe num :: Maybe Int
  isJust x

-- Menu de la información del hotel
hotelInfo :: IO ()
hotelInfo = do
  putStrLn "\t\tInformación del hotel"
  putStrLn "1- Modificar información"
  putStrLn "2- Imprimir información"
  putStrLn "3- Salir"
  putStr ">>"
  option <- getLine
  case option of
    "1" -> modHotelInfo
    "2" -> printFile "./BD/info.csv"
    "3" -> return ()
    _ -> putStrLn "[ERROR]: La opción elegida no es válida"
  Control.Monad.when (option /= "3") hotelInfo

-- Modifica los datos del hotel
modHotelInfo :: IO ()
modHotelInfo = do
  file <- getFileData "./BD/info.csv"
  putStr "Nombre de la empresa: "
  company <- getLine

  putStr "Cedula jurídica: "
  legalId <- getLine

  putStr "Sitio web: "
  website <- getLine

  putStr "Teléfono: "
  tel <- getLine

  putStr "País: "
  country <- getLine

  putStr "Provincia: "
  province <- getLine
  writeCsv "./BD/info.csv" (take 1 file ++ [[company, legalId, website, tel, country, province]])
  putStrLn "Se ha modificado con exito"

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
  String -> -- Recibe un string con la ruta
  IO () --No retorna nada
newRoomTypePath path =
  atomicModifyIORef' roomTypeRef (const (path, ()))

-- Carga carga el tipo de cuarto
loadRoomType :: IO ()
loadRoomType = do
  putStrLn "Ingrese la ruta del archivo: "
  filePath <- getLine
  newRoomTypePath filePath

-- Ciclo para preguntar la cantidad de habitaciones por tipo
loopNumRooms :: [[String]] -- Una matriz con los tipos de habitación
  -> [[String]] -- Una matriz donde se guardan los datos
  -> Int  --Id donde empieza ej: 0
  -> IO [[String]] -- Una matriz con la cantidad y el tipo ej: [["10","Individual"]...]
loopNumRooms file resMatrix id=
  if null file
    then return resMatrix
  else  do
    let typeName = head (head file)
    row  <-getNumRooms typeName id
    let newId = read (head (last row))::Int
    loopNumRooms (drop 1 file) (resMatrix++row) (newId+1)

-- Verifica y obtiene la cantidad de habitaciones por tipo
getNumRooms :: String -- El nombre del tipo
  -> Int --Id
  -> IO [[String]] -- Una lista con la cantidad de habitaciones y el tipo ej:["10",Individual]
getNumRooms typeName id= do
    printf "Ingrese la cantidad de habitaciones para %s: " typeName
    num<-getLine
    if not (isInt num) then
      do
        putStrLn "[Error]: No se ingresó un número"
        getNumRooms typeName id
      else do
        let lastId = id + read num::Int
        return [[show x,typeName] | x<-[id..(lastId-1)]]

numRoomsByType :: IO ()
numRoomsByType = do
  numRoomsFile <- noHeaderData "./BD/Rooms.csv"
  if null numRoomsFile then do
    path <- readRoomTypePath
    fileData <- noHeaderData path
    header<- getHeaderData "./BD/Rooms.csv"
    putStrLn "Asignar Cantidad de habitaciones por tipo"
    numRoomsData  <- loopNumRooms fileData [] 1
    let newData = header:numRoomsData
    writeCsv "./BD/Rooms.csv" newData
  else putStrLn "[INFO]: Ya se han asignado la cantidad de habitaciones por tipo"


chargeRates = putStrLn "Carga de Tarifas"

consultReservations = putStrLn "Consultar Reservaciones"

consultInvoice = putStrLn "Consulta de facturas"

occupancyStatistics = putStrLn "Estadísticas de ocupación"

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
  case option of
    "1" -> hotelInfo
    "2" -> loadRoomType
    "3" -> numRoomsByType
    "4" -> chargeRates
    "5" -> consultReservations
    "6" -> consultInvoice
    "7" -> occupancyStatistics
    "8" -> return ()
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

        if isNothing entryDate
          then do
            putStrLn "[Error]: El formato de la fecha es incorrecto."
            loop
          else return entryDateStr
  loop -- start the first iteration

-- Obtiene el rango de fecha de reservacion y los valida.
-- Retorna una lista con las fechas de reservacion.
getDateReservation :: IO [String]
getDateReservation = do
  let loop = do
        entryDate <- getDate
        departDate <- getDate

        -- Verifica si la fecha inicial es mayor a la final
        if entryDate > departDate
          then do
            putStrLn "[Error]: La fecha inicial debe ser menor a la fecha final."
            loop
          else return [entryDate, departDate]
  loop

-- Obtiene la cantidad de niños y adultos
-- Retorna una lista con un string de la cantidad de adultos en la posicion 0 y los niños en la 1
getNumAdultChild :: IO [String]
getNumAdultChild = do
  let loopAdult = do
        putStr "\nCantidad de adultos: "
        numAdults <- getLine
        if not (isInt numAdults)
          then do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            loopAdult
          else return numAdults

  let loopChild = do
        putStr "\nCantidad de niños: "
        numChildren <- getLine
        if not (isInt numChildren)
          then do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            loopChild
          else return numChildren
  numadult <- loopAdult
  numChild <- loopChild
  return [numadult, numChild]

-- Obtiene el tipo de habitación, la cantidad de adultos y niños huespedes
-- Retorna una lista de strings con el input de los datos.
getNumAdChRoomType :: IO [String]
getNumAdChRoomType = do
  -- Loop que verifica si el tipo de habitación es válido
  let loopRoom = do
        putStr "\nSeleccione el tipo de habitación: "
        roomType <- getLine
        if not (isInt roomType)
          then do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            loopRoom
          else return roomType

  -- Loop que verifica si es número
  let loopNumAdult = do
        putStr "\nCantidad de huéspedes adultos: "
        numAdultGuests <- getLine
        if not (isInt numAdultGuests)
          then do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            loopNumAdult
          else return numAdultGuests

  -- Loop que verifica si es número
  let loopNumChild = do
        putStr "\nCantidad de huéspedes niños: "
        numChildGuests <- getLine
        if not (isInt numChildGuests)
          then do
            putStrLn "[Error]: Lo ingresado no es un número entero."
            loopNumChild
          else return numChildGuests

  roomType <- loopRoom
  numAdult <- loopNumAdult
  numChild <- loopNumChild
  return [roomType, numAdult, numChild]

saveReservation ::
  [String] -> --Lista de strings
  IO () --Guarda los datos de reservación en un csv
saveReservation reservationData = do
  reservHistory <- getFileData "./BD/Reservation.csv"
  let lastId = head (last reservHistory)
  if not (isInt lastId)
    then addLineCsv "./BD/Reservation.csv" ("0" : reservationData)
    else do
      let id = (read lastId :: Int) + 1
      addLineCsv "./BD/Reservation.csv" (show id : reservationData)

reservation :: IO ()
reservation = do
  putStrLn "\t\tReservación"

  dateRange <- getDateReservation
  numAdultChild <- getNumAdultChild
  let numAdult = read (head numAdultChild) :: Int
  let numChild = read (numAdultChild !! 1) :: Int

  putStr "Nombre de quien reserva: "
  name <- getLine

  roomTypeData <- getNumAdChRoomType
  let numAdultGuest = read (roomTypeData !! 1) :: Int
  let numChildGuest = read (roomTypeData !! 2) :: Int

  -- Verifica si el total de huéspedes suma el total de adultos y niños.
  let numChildAdults = numAdult + numChild
  let numChildAdultsGuests = numAdultGuest + numChildGuest
  when (numChildAdults /= numChildAdultsGuests && numChildAdults > 0) $
    do
      putStrLn "El número de huéspedes y niños no es el mismo que la cantidad de adultos y niños o no son mayores a 0"
      reservation
  let reservationData = [name] ++ dateRange ++ numAdultChild ++ roomTypeData
  saveReservation reservationData

cancelReservation :: IO ()
cancelReservation = putStrLn "Cancelar reservación"

invoiceReservation :: IO ()
invoiceReservation = putStrLn "Facturar reservación"

menuGeneral :: IO ()
menuGeneral = do
  putStrLn "\t\tOpciones Generales"
  putStrLn "1- Reservación"
  putStrLn "2- Cancelar reservación"
  putStrLn "3- Facturar reservación"
  putStrLn "4- Salir"
  putStr ">>"
  option <- getLine
  case option of
    "1" -> reservation
    "2" -> cancelReservation
    "3" -> invoiceReservation
    "4" -> return ()
    _ -> putStrLn "[ERROR]: La opción elegida no es válida"
  Control.Monad.when (option /= "4") menuGeneral
