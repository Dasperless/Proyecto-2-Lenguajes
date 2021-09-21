import qualified Includes.File
import System.Exit (exitSuccess)
import Includes.File (printFile)
hotelInfo = do
    printFile "./BD/info.csv" 

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

menu :: IO ()
menu = do
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
                    "8" -> exitSuccess
                    _ -> putStrLn "[ERROR]: La opción elegida no es válida"

    putStrLn option
    menu

