module Menu where

import Data.List
import Data.Map ()
import qualified Data.Map as Map
import DataTypes
  ( BanderaAjustar (AJUSTAR, NOAJUSTAR),
    BanderaSeparar (NOSEPARAR, SEPARAR),
    HypMap,
  )
import System.IO
import TextAligning (separarYAlinear)
import Prelude hiding (filter, lookup, map, null)

type Estado = HypMap

hypMap :: HypMap
hypMap =
  Map.fromList
    []

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do
  mainloop (hypMap)

-- Ciclo de ejecución:
--  1. Recibe un Estado
--  2. lee un comando
--  3. Ejecuta un comando que produce un nuevo Estado
--  4. Se invoca recursivamente con el nuevo Estado.
mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens = words inpStr
  let comando = tokens !! 0
  -- Check for command
  case comando of
    "load" -> do
      -- Get file name
      let nombreArchivo = (tokens !! 1)
      -- Create handle
      inh <- openFile nombreArchivo ReadMode
      -- New state
      nuevoEstado <- loadDiccionario inh estado
      hClose inh
      putStrLn (("Diccionario cargado ") ++ (show (length (Map.keys nuevoEstado))) ++ " palabras cargadas")
      mainloop nuevoEstado
    "show" -> do
      putStrLn (drop 9 (show estado))
      mainloop estado
    "ins" -> do
      let palabra = (tokens !! 1)
      -- Insertar en el nuevo estado la nueva palabra
      let nuevoEstado = Map.insert palabra (words [if c == '-' then ' ' else c | c <- (tokens !! 2)]) estado
      putStrLn $ "Palabra " ++ palabra ++ " agregada"
      mainloop nuevoEstado
    "save" -> do
      let fileName = (tokens !! 1)
      -- Crear file handle
      outh <- openFile fileName WriteMode
      -- Escribir en el archivo de salida el diccionario
      saveFile outh (Map.toList estado)
      hClose outh
      putStrLn (("Diccionario cargado ") ++ (show (length (Map.keys estado))) ++ " palabras cargadas")
      mainloop estado
    "split" -> do
      -- Obtener datos de entrada
      let longitud = read (tokens !! 1) :: Int
      let separar = if (tokens !! 2) == "n" then NOSEPARAR else SEPARAR
      let ajustar = if (tokens !! 3) == "n" then NOAJUSTAR else AJUSTAR
      let texto = intercalate " " (drop 4 tokens)
      -- Separar y alinear texto
      let strings = separarYAlinear estado longitud separar ajustar texto
      -- -- Imprimir texto
      printSeparatedStrings strings
      mainloop estado
    "splitf" -> do
      -- Obtener datos de entrada
      let longitud = read (tokens !! 1) :: Int
      let separar = if (tokens !! 2) == "n" then NOSEPARAR else SEPARAR
      let ajustar = if (tokens !! 3) == "n" then NOAJUSTAR else AJUSTAR
      -- Obtener nombre del archivo fuente
      let inputFile = tokens !! 4
      -- Create handle
      inh <- openFile inputFile ReadMode
      -- Get file contents
      inpuString <- loadInputFile inh ""
      let resultStrings = separarYAlinear estado longitud separar ajustar (drop 1 inpuString)
      hClose inh
      if (length tokens) > 5
        then do
          let outputFile = tokens !! 5
          outh <- openFile outputFile WriteMode
          writeResultToFile outh resultStrings
        else do printSeparatedStrings resultStrings
      mainloop estado
    "exit" -> do
      putStrLn "Saliendo..."
    _ -> do
      putStrLn $ "Comando desconocido (" ++ comando ++ "): '" ++ inpStr ++ "'"
      mainloop estado

-- Recibe el estado actual, el token y sus silabas
-- Agrega un token nuevo al diccionario
addToken :: Estado -> String -> [String] -> Estado
addToken estado token silabas =
  -- Check for diccionary member
  if Map.member token estado
    then estado
    else Map.insert token silabas estado

-- Recibe un handle de archivo de salida y una lista con la separacion
-- de cada palabra
-- Guarda en el archivo de salida el diccionario actual
saveFile :: Handle -> [(String, [String])] -> IO ()
saveFile _ [] = return ()
saveFile outh ((k, v) : kvs) = do
  -- Write line to file
  hPutStrLn outh $ k ++ " " ++ (intercalate "-" v)
  saveFile outh kvs

-- Recibe un handle y una lista de strings
-- Escribe en un archivo cada elemento de la lista
-- en una linea del archivo
writeResultToFile :: Handle -> [String] -> IO ()
writeResultToFile _ [] = return ()
writeResultToFile outh (x : xs) = do
  hPutStrLn outh x
  writeResultToFile outh xs

-- Recibe el handle del archivo y el estado actual
-- Carga el diccionario a partir de un handle del archivo
loadDiccionario :: Handle -> Estado -> IO Estado
loadDiccionario inh estado = do
  ineof <- hIsEOF inh
  if ineof
    then return estado
    else do
      inpStr <- hGetLine inh
      let fileLine = words (inpStr)
      let silabas = fileLine !! 1
      -- Insert new slot for diccionary
      let nuevoEstado = addToken estado (head fileLine) (words [if c == '-' then ' ' else c | c <- silabas])
      loadDiccionario inh nuevoEstado

-- Recibe el handle del archivo de entrada para el texto
-- Retorna un string con todas las lineas
loadInputFile :: Handle -> String -> IO String
loadInputFile inh string = do
  -- Verificar EOF
  ineof <- hIsEOF inh
  if ineof
    then return string
    else do
      -- Get line
      inpStr <- hGetLine inh
      let nuevoString = string ++ " " ++ inpStr
      loadInputFile inh nuevoString

-- Recibe una lista de strings
-- Imprime un string por linea en la terminal
printSeparatedStrings :: [String] -> IO ()
printSeparatedStrings [] = do putStr ""
printSeparatedStrings (x : xs) = do
  putStrLn x
  printSeparatedStrings xs
