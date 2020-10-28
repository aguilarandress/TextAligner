module Menu where

import Data.Map.Lazy (fromList)
import DataTypes (HypMap)
import System.IO (IO, getLine, putStr, putStrLn)
import TextAligning ()
import Prelude hiding (filter, lookup, map, null)

type Estado = HypMap

-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do
  mainloop (fromList [])

-- Ciclo de ejecución:
--     1. Recibe un Estado
--     2. lee un comando
--     3. Ejecuta un comando que produce un nuevo Estado
--     4. Se invoca recursivamente con el nuevo Estado.
mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens = words inpStr
  let comando = tokens !! 0
  -- Check for command
  case comando of
    "leer" -> do
      putStrLn ">>> Hola mundo"
      mainloop estado
    "fin" -> do
      putStrLn "Saliendo..."
    _ -> do
      putStrLn $ "Comando desconocido (" ++ comando ++ "): '" ++ inpStr ++ "'"
      mainloop estado
