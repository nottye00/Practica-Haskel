import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.DeepSeq (deepseq)

-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    horaEntrada  :: UTCTime,
    horaSalida   :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada idNuevo tiempo lista =
    Estudiante idNuevo tiempo Nothing : lista

-- Función para registrar la salida de un estudiante
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida idBuscado tiempo =
    map (\est -> if idEstudiante est == idBuscado
                 then est { horaSalida = Just tiempo }
                 else est)

-- Función para buscar a un estudiante por su ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante idBuscado =
    find (\est -> idEstudiante est == idBuscado && esActivo est)
  where
    esActivo est = case horaSalida est of
                     Nothing -> True
                     _       -> False

-- Calcular tiempo transcurrido (desde entrada hasta ahora)
calcularTiempoEnUniversidad :: Estudiante -> IO NominalDiffTime
calcularTiempoEnUniversidad estudiante = do
    tiempoActual <- getCurrentTime
    return $ diffUTCTime tiempoActual (horaEntrada estudiante)

-- Guardar información de estudiantes en archivo "University.txt"
guardarUniversidad :: [Estudiante] -> IO ()
guardarUniversidad estudiantes = do
    withFile "University.txt" WriteMode $ \h -> do
        hPutStr h (unlines (map mostrarEstudiante estudiantes))
    putStrLn "Información guardada en 'University.txt'."

-- Cargar datos desde "University.txt"
cargarUniversidad :: IO [Estudiante]
cargarUniversidad = do
    contenido <- withFile "University.txt" ReadMode $ \h -> do
        c <- hGetContents h
        c `deepseq` return c
    let lineas = lines contenido
    if null lineas
       then return []
       else return (map readEstudiante lineas)

-- Parse manual de String a Estudiante
readEstudiante :: String -> Estudiante
readEstudiante = read

-- Mostrar información de un estudiante en formato String
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante (Estudiante idEst eTime sTime) =
    "Estudiante {idEstudiante = \"" ++ idEst
    ++ "\", horaEntrada = "   ++ show eTime
    ++ ", horaSalida = "      ++ maybe "Nothing" show sTime
    ++ "}"

-- Listar estudiantes
listarEstudiantes :: [Estudiante] -> IO ()
listarEstudiantes [] = putStrLn "No hay estudiantes registrados."
listarEstudiantes ests = do
    putStrLn "Estudiantes registrados:"
    mapM_ (putStrLn . mostrarEstudiante) ests

-- main y ciclo principal
main :: IO ()
main = do
    estudiantes <- cargarUniversidad
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes de la Universidad!"
    cicloPrincipal estudiantes

cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal lista = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            tActual <- getCurrentTime
            let nuevaLista = registrarEntrada idEst tActual lista
            putStrLn $ "Estudiante con ID " ++ idEst ++ " ha ingresado."
            guardarUniversidad nuevaLista
            cicloPrincipal nuevaLista

        "2" -> do
            putStrLn "Ingrese el ID del estudiante:"
            idEst <- getLine
            tActual <- getCurrentTime
            let nuevaLista = registrarSalida idEst tActual lista
            putStrLn $ "Estudiante con ID " ++ idEst ++ " ha salido."
            guardarUniversidad nuevaLista
            cicloPrincipal nuevaLista

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            idEst <- getLine
            case buscarEstudiante idEst lista of
                Just est -> do
                    tiempoTotal <- calcularTiempoEnUniversidad est
                    putStrLn $ "El estudiante con ID " ++ idEst ++ " sigue en la universidad."
                    putStrLn $ "Tiempo transcurrido: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado o ya salió."
            cicloPrincipal lista

        "4" -> do
            listarEstudiantes lista
            cicloPrincipal lista

        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Intente de nuevo."
            cicloPrincipal lista