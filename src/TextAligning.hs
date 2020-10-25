module TextAligning where

import Data.List ()
import Data.Map ()
import qualified Data.Map as Map
import DataTypes (HypMap, Line, Token (..))

-- @params  Recibe un token
-- @desc    Convierte un token en su versión de string
-- @returns El token convertido en string
tokenToString :: Token -> String
tokenToString (Word token) = token ++ " "
tokenToString (Blank) = " "
tokenToString (HypWord hypWord) = hypWord ++ "- "

tokenToString' :: Token -> String
tokenToString' (Word token) = token
tokenToString' (Blank) = " "
tokenToString' (HypWord hypWord) = hypWord

-- @params  Recibe un String
-- @desc    Convierte un String en una Line (Funcion A)
-- @returns El string convertido en una Line
string2line :: String -> Line
string2line texto =
  if length (texto) == 0
    then []
    else map Word (splitString texto)

-- @params  Recibe un String
-- @desc    Toma un string y lo separa por espacios en una lista de strings
-- @returns La lista de strings seprados por espacios
splitString :: String -> [String]
splitString [] = []
splitString (x : xs)
  | isBlank x = splitString xs
  | otherwise = waitForBlank (x : xs) : splitString (restOfTheString)
  where
    restOfTheString = drop (length (waitForBlank (x : xs))) xs

-- @params  Recibe un char
-- @desc    Verifica si el char es un espacio en blanco
-- @returns Un valor booleano que determina si el char es un espacio en blanco
isBlank :: Char -> Bool
isBlank char = if char == ' ' then True else False

-- @params  Recibe un String
-- @desc    Toma un string e itera sobre el hasta encontrar un espacio en blanco
-- @returns La seccioón del string hasta un espacio
waitForBlank :: String -> String
waitForBlank [] = []
waitForBlank (x : xs)
  | isBlank x = []
  | otherwise = x : waitForBlank xs

-- @params  Recibe una Line
-- @desc    Toma una line y lo convierte en un string
-- @returns El string formado a partir de una line
line2string :: Line -> String
line2string [] = ""
line2string line =
  let stringResult = concatTokens (line) (0) (length line)
   in if last stringResult == ' '
        then drop 0 (take (length (stringResult) - 1) (stringResult))
        else stringResult

-- Concats tokens into a string
concatTokens ::
  Line ->
  Int ->
  Int ->
  String
concatTokens (x : xs) (currentIndex) lineLength
  | (x : xs) == [] = []
  | currentIndex == lineLength - 1 =
    if tokenToString (x) == " "
      then ""
      else drop 0 (take (length (tokenToString x) - 1) (tokenToString x))
  | otherwise =
    if (tokenToString x == " " && currentIndex == 0)
      then concatTokens (xs) (currentIndex + 1) lineLength
      else (tokenToString x) ++ concatTokens (xs) (currentIndex + 1) lineLength

-- Function #3
tokenLength :: Token -> Int
tokenLength Blank = 1
tokenLength token = length (tokenToString token) - 1

-- Function #4
lineLength :: Line -> Int
lineLength [] = -1
lineLength (x : xs) =
  tokenLength x + 1 + lineLength xs

-- Function #5
breakLine :: Int -> Line -> (Line, Line)
breakLine lineLimit line
  | line == [] = ([], [])
  | otherwise =
    if lineLength line <= lineLimit
      then (line, [])
      else (limittedLine, rest)
  where
    limittedLine = concatTokensWithLimit line lineLimit 0
    rest = drop (length limittedLine) line

-- Dado un limite de tokens retorna una lista
-- con un maximo de palabras
concatTokensWithLimit :: Line -> Int -> Int -> Line
concatTokensWithLimit (x : xs) limit currentLength
  | (x : xs) == [] = []
  | otherwise =
    if currentLength + headLength > limit
      then []
      else (x : concatTokensWithLimit (xs) (limit) (newLength))
  where
    headLength = length (tokenToString x) - 1
    newLength = headLength + currentLength

-- Function #6
mergers :: [String] -> [(String, String)]
mergers [] = []
mergers stringList
  | length stringList == 1 = []
  | otherwise = getTotalConcats stringList 0

-- Obtiene todos los concats posibles de una lista de strings
getTotalConcats :: [String] -> Int -> [(String, String)]
getTotalConcats strings index
  | strings == [] = []
  | otherwise =
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
convertToHyphennedWord (" ", " ") = (Blank, Blank)
convertToHyphennedWord stringTuple =
  (HypWord (fst stringTuple), Word (snd stringTuple))

-- Function 8
lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks diccionario limit line =
  [([Word "Hello"], [Word "World"])]

insertBlanks :: Int -> Line -> Line
insertBlanks 0 line = line
insertBlanks _ [] = []
insertBlanks numberOfBlanks line
  | length line == 1 = line
  | otherwise =
    let blanksPlaceholder = [[] | i <- [1 .. ((length line) - 1)]]
        blanksList = createBlanks blanksPlaceholder numberOfBlanks 0
     in addBlanksToWords line blanksList 0

addBlanksToWords :: Line -> [[Token]] -> Int -> Line
addBlanksToWords [] _ _ = []
addBlanksToWords line [] _ = line
addBlanksToWords (x : xs) blanks currentIndex =
  if xs == []
    then [x]
    else [x] ++ (blanks !! currentIndex) ++ addBlanksToWords xs blanks (currentIndex + 1)

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
