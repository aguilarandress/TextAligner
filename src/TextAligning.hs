module TextAligning where

import Data.List ()
import Data.Map ()
import qualified Data.Map as Map
import DataTypes (BanderaAjustar (..), BanderaSeparar (..), HypMap, Line, Token (..))

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
    -- Toma un string y lo separa por espacios en blanco
    splitString :: String -> [String]
    splitString [] = []
    splitString (x : xs)
      | isBlank x = splitString xs
      | otherwise = (takeWhile (/= ' ') (x : xs)) : splitString (restOfTheString)
      where
        -- Se toma el string desde el espacio encontrado
        restOfTheString = dropWhile (/= ' ') (x : xs)

-- Recibe una line y la convierte en su forma de string (Funcion B)
line2string :: Line -> String
line2string [] = []
line2string line = take ((length stringResult) - 1) stringResult
  where
    -- Eliminar blancos al inicio
    lineWithoutBlanksAtStart = dropWhile (== Blank) (line)
    -- Eliminar blancos al final
    lineWithoutBlanksAtEnd = reverse (dropWhile (== Blank) (reverse lineWithoutBlanksAtStart))
    stringResult = concat (map tokenToString lineWithoutBlanksAtEnd)

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
  where
    -- Obtiene todos los concats posibles de una lista de strings
    getTotalConcats :: [String] -> Int -> [(String, String)]
    getTotalConcats [] _ = []
    getTotalConcats strings index =
      -- Detenerse cuando se llega al ultimo elemento
      if index >= length (strings) - 1
        then []
        else (leftSideConcat, rightSideConcat) : getTotalConcats strings (index + 1)
      where
        leftSideConcat = concat (take (index + 1) (strings))
        rightSideConcat = concat (drop (index + 1) (strings))

-- Recibe un diccionario de palabras y un token (Funcion G)
-- Retorna las palabras de la forma (HypWord, Word)
hyphenate :: HypMap -> Token -> [(Token, Token)]
hyphenate diccionario word =
  let wordWithoutPunctuation = fst (extractPunctuation (tokenToString' word))
      punctuation = snd (extractPunctuation (tokenToString' word))
      -- Obtener combinaciones de los strings con el map
      stringCombinations = mergers (diccionario Map.! wordWithoutPunctuation)
      -- Crear palabras con el hyphen
      hyphennedWords = map convertToHyphennedWord stringCombinations
      -- Recibe un char y retorna un valor booleano dependiendo
      -- de si es un simbolo de puntuacion o no
      isPunctuationSymbol :: Char -> Bool
      isPunctuationSymbol char =
        if char == '.' || char == ';' || char == ','
          then True
          else False
      isPunctuationSymbol' :: Char -> Bool
      isPunctuationSymbol' char =
        if char == '.' || char == ';' || char == ','
          then False
          else True
      -- Recibe una lista de pares de tokens y la puntuacion
      -- Retorna la lista de pares de tokens con su puntuacion al final
      addPunctuation :: [(Token, Token)] -> String -> [(Token, Token)]
      addPunctuation [] _ = []
      addPunctuation (x : xs) punctuation =
        -- Agregar puntuacion a la palabra
        let newWord = Word (tokenToString' (snd x) ++ punctuation)
         in (fst x, newWord) : addPunctuation xs punctuation
      -- Recibe un string
      -- Retorna un par en donde el primer campo es la palabra y el segundo es la puntuacion
      extractPunctuation :: String -> (String, String)
      extractPunctuation [] = ([], [])
      extractPunctuation string = (reverse (dropWhile (isPunctuationSymbol) (reverse string)), dropWhile (isPunctuationSymbol') string)
      -- Recibe un par de strings
      -- Retorna un par de la forma (HypWord, Word)
      convertToHyphennedWord :: (String, String) -> (Token, Token)
      convertToHyphennedWord stringTuple =
        (HypWord (fst stringTuple), Word (snd stringTuple))
   in -- Verificar que la palabra que se encuentre en el diccionario
      if Map.member wordWithoutPunctuation diccionario
        then -- Agregar signos de puntuacion originales
          addPunctuation hyphennedWords punctuation
        else []

-- Recibe un diccionario de palabras, un limite de linea y una linea (Funcion h)
-- Retorna las distintas maneras de partir la linea de forma que
-- cumpla con el limite establecido
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
        -- Recibe una line partida en dos y los hyphens posibles para la ultima palabra
        -- Retorna la linea partida con los hyphens
        addHyphensToSplittedLine :: (Line, Line) -> [(Token, Token)] -> [(Line, Line)]
        addHyphensToSplittedLine _ [] = []
        addHyphensToSplittedLine splittedLine (x : xs) =
          let leftSideOfLine = fst splittedLine ++ [fst x]
              -- Eliminar la primer parte de la segunda linea para agregar la segunda parte
              -- de la palabra partida en 2
              rightSideOfLine = [snd x] ++ drop 1 (snd splittedLine)
           in (leftSideOfLine, rightSideOfLine) : (addHyphensToSplittedLine splittedLine xs)
     in -- Verificar si no hay manera de partir la ultima palabra
        if posibleHyphens == []
          then [splittedLine]
          else -- Retornar aquellas lineas que si cumplen con el limite
            splittedLine : (filter (\x -> lineLength (fst x) <= limit) (addHyphensToSplittedLine splittedLine posibleHyphens))

-- Recibe un numero de blanks y una line (Funcion i)
-- Inserta el numero de blanks entre cada token de la line
insertBlanks :: Int -> Line -> Line
insertBlanks 0 line = line
insertBlanks _ [] = []
insertBlanks numberOfBlanks line
  | length line == 1 = line
  | otherwise =
    -- Placeholder para los Blanks entre las palabras
    let blanksPlaceholder = replicate ((length line) - 1) []
        -- Generar los blanks que van entre las palabras
        blanksList = createBlanks blanksPlaceholder numberOfBlanks 0
        -- Recibe una lista de blanks, un numero de blanks y un indice
        -- Genera listas de blanks hasta que se agote el numero de blanks
        createBlanks :: [[Token]] -> Int -> Int -> [[Token]]
        createBlanks [] _ _ = []
        createBlanks listOfBlanks numberOfBlanks currentIndex
          | numberOfBlanks == 0 = listOfBlanks
          | otherwise =
            -- Agregar Blank a lista de Blanks actual
            let currentElementWithBlank = [(listOfBlanks !! currentIndex) ++ [Blank]]
                elementsBeforeCurrent = (take currentIndex listOfBlanks)
                elementsAfterCurrent = (drop (currentIndex + 1) listOfBlanks)
                newListOfBlanks = (elementsBeforeCurrent) ++ (currentElementWithBlank) ++ (elementsAfterCurrent)
             in if (length listOfBlanks) - 1 == currentIndex
                  then createBlanks (newListOfBlanks) (numberOfBlanks - 1) 0
                  else createBlanks (newListOfBlanks) (numberOfBlanks - 1) (currentIndex + 1)
        -- Recibe una line y una lista de blanks
        -- Le agrega a cada word sus respectivos blanks
        addBlanksToWords :: Line -> [[Token]] -> Int -> Line
        addBlanksToWords [] _ _ = []
        addBlanksToWords line [] _ = line
        addBlanksToWords (x : xs) blanks currentIndex =
          if xs == []
            then [x]
            else [x] ++ (blanks !! currentIndex) ++ addBlanksToWords xs blanks (currentIndex + 1)
     in addBlanksToWords line blanksList 0

-- Funcion que recibe recibe un string y lo separa en una lista de strings (Funcion j)
separarYalinear :: HypMap -> Int -> BanderaSeparar -> BanderaAjustar -> String -> [String]
separarYalinear _ _ _ _ [] = []
separarYalinear diccionario limite separar ajustar string
  -- NOSEPARAR Y NOAJUSTAR
  | separar == NOSEPARAR && ajustar == NOAJUSTAR =
    map line2string (brokenLines)
  -- NOSEPARAR Y AJUSTAR
  | separar == NOSEPARAR && ajustar == AJUSTAR =
    map line2string ((adjustLines limite (init brokenLines)) ++ [last brokenLines])
  -- SEPARAR Y NOAJUSTAR
  | separar == SEPARAR && ajustar == NOAJUSTAR =
    map line2string (brokenLinesWithHypen)
  -- SEPARAR Y AJUSTAR
  | otherwise = map line2string ((adjustLines limite (init brokenLinesWithHypen)) ++ [last brokenLinesWithHypen])
  where
    brokenLines = breakIntoLines limite (string2line string)
    brokenLinesWithHypen = breakIntoLines' limite (string2line string)
    -- Ajusta las lineas de manera que encajen con el limite dado
    adjustLines :: Int -> [Line] -> [Line]
    adjustLines _ [] = []
    adjustLines limit (x : xs) = insertBlanks (limit - (lineLength x)) x : adjustLines limit xs
    -- Separa el string de manera que no sobrepase el limite ingresado
    -- Esta funcion no separa las palabras
    breakIntoLines :: Int -> Line -> [Line]
    breakIntoLines _ [] = []
    breakIntoLines limit line =
      let brokenLine = breakLine limit line
       in (fst brokenLine) : breakIntoLines limit (snd brokenLine)
    -- Separa el string de manera que no sobrepase el limite ingresado
    -- Esta funcion separa las palabras
    breakIntoLines' :: Int -> Line -> [Line]
    breakIntoLines' _ [] = []
    breakIntoLines' limit line =
      let brokenLines = lineBreaks diccionario limit line
          biggestLine = (last brokenLines)
       in (fst biggestLine) : breakIntoLines' limit (snd biggestLine)
