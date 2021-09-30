import Control.Monad (unless, when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Time (UniversalTime, getCurrentTime)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Includes.Date (parseDate)
import Includes.File (addLineCsv, getFileData, getHeaderData, newRoomTypePath, noHeaderData, printFile, readRoomTypePath, roomTypeRef, writeCsv, printTablePos, printRow)
import Includes.Calc (getRateList)

-- Verifica si un número es entero
isInt ::
  String -> -- Obtiene un string
  Bool --Retorna True si representa un entero, False en caso contrario
isInt num = do
  let x = readMaybe num :: Maybe Int
  isJust x

-- Verifica que el input sea un entero
inputInt :: IO String
inputInt = do
  input<-getLine
  if isInt input
    then return input
  else do
    putStrLn "[Error]: El dato ingresado no es un entero"
    inputInt

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

-- Carga carga el tipo de cuarto
loadRoomType :: IO ()
loadRoomType = do
  putStrLn "Ingrese la ruta del archivo: "
  filePath <- getLine
  newRoomTypePath filePath

-- Ciclo para preguntar la cantidad de habitaciones por tipo
loopNumRooms ::
  [[String]] -> -- Una matriz con los tipos de habitación
  [[String]] -> -- Una matriz donde se guardan los datos
  Int -> --Id donde empieza ej: 0
  IO [[String]] -- Una matriz con la cantidad y el tipo ej: [["10","Individual"]...]
loopNumRooms file resMatrix id =
  if null file
    then return resMatrix
    else do
      let typeName = head (head file)
      row <- getNumRooms typeName id
      let newId = read (head (last row)) :: Int
      loopNumRooms (drop 1 file) (resMatrix ++ row) (newId + 1)

-- Verifica y obtiene la cantidad de habitaciones por tipo
getNumRooms ::
  String -> -- El nombre del tipo
  Int -> --Id
  IO [[String]] -- Una lista con la cantidad de habitaciones y el tipo ej:["10",Individual]
getNumRooms typeName id = do
  printf "Ingrese la cantidad de habitaciones para %s: " typeName
  num <- getLine
  if not (isInt num)
    then do
      putStrLn "[Error]: No se ingresó un número"
      getNumRooms typeName id
    else do
      let lastId = id + read num :: Int
      return [[show x, typeName] | x <- [id .. (lastId - 1)]] --Lista por comprensión

-- Asigna la cantidad de cuartos por tipo.
numRoomsByType :: IO ()
numRoomsByType = do
  numRoomsFile <- noHeaderData "./BD/Rooms.csv"
  if null numRoomsFile
    then do
      path <- readRoomTypePath
      fileData <- noHeaderData path
      header <- getHeaderData "./BD/Rooms.csv"
      putStrLn "Asignar Cantidad de habitaciones por tipo"
      numRoomsData <- loopNumRooms fileData [] 1
      let newData = header : numRoomsData
      writeCsv "./BD/Rooms.csv" newData
    else putStrLn "[INFO]: Ya se han asignado la cantidad de habitaciones por tipo"

-- Carga las tarifas 
chargeRates :: IO ()
chargeRates = do
  putStrLn "\t\tCarga de Tarifas"
  putStr "Ingrese la ruta del archivo: "
  path <- getLine
  file <-noHeaderData path
  let header = ["Id Tarifa","Precio"]
  newDataRates <- getChargeRates file []
  writeCsv path (header:newDataRates)

-- Obtiene el input de las tarifas
getChargeRates :: [[String]] -- ^ Una matriz con los datos del archivo
  -> [[String]] -- ^ La lista donde se va a guardar los datos de las tarifas
  -> IO [[String]]
getChargeRates file newRates = do
  if not (null file) then do
    let rateNum = head (head file)
    printf "Ingrese el valor de la tarifa %s: " rateNum
    newRate<-inputInt
    getChargeRates (tail file) (newRates++[[rateNum,newRate]])
  else return newRates

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
        putStrLn "Ingresa la fecha (dd/mm/yyyy): "
        putStr ">>"
        entryDateStr <- getLine
        let entryDate = parseDate entryDateStr

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
getNumAdChRoomType :: IO [String] -- Retorna una lista de strings con el input de los datos.
getNumAdChRoomType = do
  -- Loop que verifica si el tipo de habitación es válido
  let loopRoom = do
      printFile  "./BD/Rooms.csv"
      putStr "\nSeleccione el id de la habitacion: "
      inputInt

  -- Loop que verifica si es número
  let loopNumAdult = do
      putStr "\nCantidad de huéspedes adultos: "
      inputInt

  -- Loop que verifica si es número
  let loopNumChild = do
      putStr "\nCantidad de huéspedes niños: "
      inputInt

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

-- Imprime el recibo
receipt :: [String] -- ^ Lista con la fecha de inicio y de salida
  -> [String] -- ^ Cantidad de niños y adultos
  -> IO ()
receipt dateRange numAdultChild = do
  let header = ["id reserva","nombre de quien reserva","fecha de reservacion","fecha ingreso","fecha salida", "cantidad adultos","cantidad de ninos","total"]
  printRow header
  file <- noHeaderData "./BD/Receipt.csv"
  let dataReserv = last file
  let lastId = head dataReserv
  currentTime <- getCurrentTime
  let receiptData = [lastId]++[dataReserv!!1]++[show currentTime]++dateRange++numAdultChild
  printRow receiptData


reservation :: IO ()
reservation = do
  putStrLn "\t\tReservación"

  dateRange <- getDateReservation --Obtiene la fecha de ingreso y salida
  numAdultChild <- getNumAdultChild--Obtiene el número de niños y adultos
  let numAdult = read (head numAdultChild) :: Int --Cantidad de niños
  let numChild = read (numAdultChild !! 1) :: Int --Cantidad de adultos

  putStr "Nombre de quien reserva: "
  name <- getLine

  roomTypeData <- getNumAdChRoomType
  let roomId = head roomTypeData
  let numAdultGuest = read (roomTypeData !! 1) :: Int
  let numChildGuest = read (roomTypeData !! 2) :: Int

  -- Verifica si el total de huéspedes suma el total de adultos y niños.
  let numChildAdults = numAdult + numChild
  let numChildAdultsGuests = numAdultGuest + numChildGuest
  when (numChildAdults /= numChildAdultsGuests && numChildAdults > 0) $
    do
      putStrLn "El número de huéspedes y niños no es el mismo que la cantidad de adultos y niños o no son mayores a 0"
      reservation
  let receiptData = [name] ++ dateRange ++ numAdultChild ++ [roomId]
  saveReservation receiptData
  receipt dateRange numAdultChild

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
