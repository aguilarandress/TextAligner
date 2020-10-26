module TextAligning where

import Data.List ()
import Data.Map ()
import qualified Data.Map as Map
import DataTypes (HypMap, Line, Token (..))

-- Recibe un token y lo convierte en su version de string
-- Retorna el token con un espacio al final
tokenToString :: Token -> String
tokenToString (Word token) = token ++ " "
tokenToString (Blank) = " "
tokenToString (HypWord hypWord) = hypWord ++ "- "

-- Recibe un token y lo convierte en su version de string
-- Retorna el token con un espacio al final
tokenToString' :: Token -> String
tokenToString' (Word token) = token
tokenToString' (Blank) = " "
tokenToString' (HypWord hypWord) = hypWord ++ "-"

-- Convierte un string en una line (Funcion A)
string2line :: String -> Line
string2line [] = []
string2line texto = map Word (splitString texto)
  where
    -- Verifica si el char es un espacio en blanco (un blank)
    isBlank :: Char -> Bool
    isBlank char = if char == ' ' then True else False
    -- Obtiene el substring antes de un espacio
    waitForBlank :: String -> String
    waitForBlank [] = []
    waitForBlank (x : xs)
      | isBlank x = []
      | otherwise = x : waitForBlank xs
    -- Toma un string y lo separa por espacios en blanco
    splitString :: String -> [String]
    splitString [] = []
    splitString (x : xs)
      | isBlank x = splitString xs
      | otherwise = waitForBlank (x : xs) : splitString (restOfTheString)
      where
        -- Se toma el string desde el espacio encontrado
        restOfTheString = drop (length (waitForBlank (x : xs))) xs

-- Recibe una line y la convierte en su forma de string (Funcion B)
line2string :: Line -> String
line2string [] = []
line2string line = concatTokens (line) (0) (length line)
  where
    -- Toma los tokens de una line y los concatena
    concatTokens ::
      Line ->
      Int ->
      Int ->
      String
    concatTokens [] _ _ = []
    concatTokens (x : xs) (currentIndex) lineLength
      -- Verificar si se llego al final del string
      | currentIndex == lineLength - 1 =
        if tokenToString (x) == " "
          then []
          else tokenToString' x
      -- Verificar si hay un espacio al inicio
      | tokenToString x == " " && currentIndex == 0 =
        -- Ignorar espacio al inicio
        concatTokens (xs) (currentIndex + 1) lineLength
      -- Continuar concatenando
      | otherwise = (tokenToString x) ++ concatTokens (xs) (currentIndex + 1) (lineLength)

-- Determina el largo de un token (Funcion C)
tokenLength :: Token -> Int
tokenLength token = length (tokenToString' token)

-- Determina el largo de una line (Funcion D)
lineLength :: Line -> Int
lineLength [] = -1
lineLength (x : xs) = tokenLength x + 1 + lineLength xs

-- Recibe una linea y un limite
-- Parte una linea de modo que no sea mas
-- larga que una longitud dada (Funcion E)
breakLine :: Int -> Line -> (Line, Line)
breakLine _ [] = ([], [])
breakLine lineLimit line =
  if lineLength line <= lineLimit
    then (line, [])
    else (limittedLine, rest)
  where
    -- Obtener la linea que cumple con el limite
    limittedLine = concatTokensWithLimit line lineLimit 0
    -- Eliminar la linea limitada de la original para obtener el resto
    rest = drop (length limittedLine) line
    -- Dado un limite de tokens retorna una lista
    -- con un maximo de palabras
    concatTokensWithLimit :: Line -> Int -> Int -> Line
    concatTokensWithLimit [] _ _ = []
    concatTokensWithLimit (x : xs) limit currentLength =
      -- Verificar que el largo actual con el largo del token no sea mayor
      if currentLength + (headLength - 1) > limit
        then []
        else (x : concatTokensWithLimit (xs) (limit) (newLength))
      where
        -- Obtener el largo de la cabeza de la lista
        headLength = length (tokenToString x)
        -- Nuevo largo = currentLength + tokenActual + el espacio
        newLength = headLength + currentLength

-- Recibe una lista de strings y genera todas las formas de concatenarlas
-- (Funcion F)
mergers :: [String] -> [(String, String)]
mergers [] = []
mergers stringList
  | length stringList == 1 = []
  | otherwise = getTotalConcats stringList 0

-- Obtiene todos los concats posibles de una lista de strings
getTotalConcats :: [String] -> Int -> [(String, String)]
getTotalConcats [] _ = []
getTotalConcats strings index =
  if index >= length (strings) - 1
    then []
    else
      [(joinStringsFromIndex (take (index + 1) (strings)) (0), joinStringsFromIndex (strings) (index + 1))]
        ++ getTotalConcats strings (index + 1)

-- Concatena todos los strings de una lista desde un indice dado
joinStringsFromIndex :: [String] -> Int -> String
joinStringsFromIndex stringList index
  | stringList == [] = []
  | otherwise =
    if index >= length stringList
      then ""
      else (stringList !! index) ++ joinStringsFromIndex (stringList) (index + 1)

enHyp :: HypMap
enHyp =
  Map.fromList
    [ ("controla", ["con", "tro", "la"]),
      ("futuro", ["fu", "tu", "ro"]),
      ("presente", ["pre", "sen", "te"])
    ]

-- Function #7
hyphenate :: HypMap -> Token -> [(Token, Token)]
hyphenate diccionario word =
  let wordWithoutPunctuation = extractPunctuation (tokenToString' word)
      -- Obtener combinaciones de los strings con el map
      stringCombinations = mergers (diccionario Map.! wordWithoutPunctuation)
      -- Crear palabras con el hyphen
      hyphennedWords = map convertToHyphennedWord stringCombinations
   in -- Verificar que la palabra que se encuentre en el diccionario
      if Map.member wordWithoutPunctuation diccionario
        then -- Agregar signos de puntuacion originales
          addPunctuation hyphennedWords (getPunctuation (tokenToString' word))
        else []

addPunctuation :: [(Token, Token)] -> String -> [(Token, Token)]
addPunctuation [] _ = []
addPunctuation (x : xs) punctuation =
  -- Agregar puntuacion a la palabra
  let newWord = Word (tokenToString' (snd x) ++ punctuation)
   in (fst x, newWord) : addPunctuation xs punctuation

extractPunctuation :: String -> String
extractPunctuation [] = []
extractPunctuation (x : xs)
  | x == '.' = ""
  | otherwise = [x] ++ extractPunctuation xs

getPunctuation :: String -> String
getPunctuation [] = []
getPunctuation (x : xs)
  | x == '.' = "." ++ getPunctuation xs
  | otherwise = getPunctuation xs

convertToHyphennedWord :: (String, String) -> (Token, Token)
-- convertToHyphennedWord (" ", " ") = (Blank, Blank) -- ?
convertToHyphennedWord stringTuple =
  (HypWord (fst stringTuple), Word (snd stringTuple))

-- Function 8
lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks _ 0 _ = []
lineBreaks _ _ [] = []
lineBreaks diccionario limit line
  -- Si el limite es mayor a la line, se retorna la misma linea
  | limit >= lineLength line = [(line, [])]
  | otherwise =
    -- Obtener linea partida con breakLine
    let splittedLine = breakLine limit line
        -- Obtener la palabra que se va a partir
        wordToBeHyphenned = head (snd splittedLine)
        -- Utilizar hyphenate para buscar las posibles formas de partir la palabra
        posibleHyphens = hyphenate diccionario wordToBeHyphenned
     in -- Verificar si no hay manera de partir la ultima palabra
        if posibleHyphens == []
          then [splittedLine]
          else splittedLine : (filter (\x -> lineLength (fst x) <= limit) (addHyphensToSplittedLine splittedLine posibleHyphens))

addHyphensToSplittedLine :: (Line, Line) -> [(Token, Token)] -> [(Line, Line)]
addHyphensToSplittedLine _ [] = []
addHyphensToSplittedLine splittedLine (x : xs) =
  let leftSideOfLine = fst splittedLine ++ [fst x]
      rightSideOfLine = [snd x] ++ drop 1 (snd splittedLine)
   in (leftSideOfLine, rightSideOfLine) : (addHyphensToSplittedLine splittedLine xs)

-- Function #9
insertBlanks :: Int -> Line -> Line
insertBlanks 0 line = line
insertBlanks _ [] = []
insertBlanks numberOfBlanks line
  | length line == 1 = line
  | otherwise =
    let blanksPlaceholder = [[] | i <- [1 .. ((length line) - 1)]]
        blanksList = createBlanks blanksPlaceholder numberOfBlanks 0
     in addBlanksToWords line blanksList 0

createBlanks :: [[Token]] -> Int -> Int -> [[Token]]
createBlanks [] _ _ = []
createBlanks listOfBlanks numberOfBlanks currentIndex
  | numberOfBlanks == 0 = listOfBlanks
  | otherwise =
    let currentElement = listOfBlanks !! currentIndex
        newListOfBlanks = (take currentIndex listOfBlanks) ++ [currentElement ++ [Blank]] ++ (drop (currentIndex + 1) listOfBlanks)
     in if (length listOfBlanks) - 1 == currentIndex
          then createBlanks (newListOfBlanks) (numberOfBlanks - 1) 0
          else createBlanks (newListOfBlanks) (numberOfBlanks - 1) (currentIndex + 1)

addBlanksToWords :: Line -> [[Token]] -> Int -> Line
addBlanksToWords [] _ _ = []
addBlanksToWords line [] _ = line
addBlanksToWords (x : xs) blanks currentIndex =
  if xs == []
    then [x]
    else [x] ++ (blanks !! currentIndex) ++ addBlanksToWords xs blanks (currentIndex + 1)
