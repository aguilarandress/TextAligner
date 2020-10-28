module Menu where

import Data.Map ()
import qualified Data.Map as Map
import DataTypes (HypMap)
import System.IO
import TextAligning ()
import Prelude hiding (filter, lookup, map, null)

type Estado = HypMap

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do
  mainloop (Map.fromList [])

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
      putStrLn (show estado)
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
      let nuevoEstado = addToken estado (head fileLine) (words [if c == ',' then ' ' else c | c <- silabas])
      loadDiccionario inh nuevoEstado
