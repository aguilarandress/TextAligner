module TextAligning where

import Data.List ()
import DataTypes (Line, Token (..))

-- Converts a token to a string
tokenToString :: Token -> String
tokenToString (Word token) = token ++ " "
tokenToString (Blank) = " "
tokenToString (HypWord hypWord) = hypWord ++ "- "

-- Funcion #1
-- Casts a string to a line
string2line :: String -> Line
string2line texto =
  if length (texto) == 0
    then []
    else map Word (splitString texto)

-- Splits a string into a list of strings
splitString :: String -> [String]
splitString [] = []
splitString (x : xs)
  | isBlank x = splitString xs
  | otherwise = waitForBlank (x : xs) : splitString (drop (length (waitForBlank (x : xs))) xs)

-- Checks if character is a space
isBlank :: Char -> Bool
isBlank currentChar = if currentChar == ' ' then True else False

-- Loops through string until a space
waitForBlank :: String -> String
waitForBlank [] = []
waitForBlank (x : xs)
  | isBlank x = []
  | otherwise = x : waitForBlank xs

-- Function #2
-- Casts a line to a string
line2string :: Line -> String
line2string [] = ""
line2string line =
  let stringResult = concatTokens (line) (0) (length line)
   in if last stringResult == ' '
        then drop 0 (take (length (stringResult) - 1) (stringResult))
        else stringResult

-- Concats tokens into a string
-- TODO: Revisar esta function en caso de lista vacia
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
lineLength [] = 0
lineLength (x : xs) =
  tokenLength x + lineLength xs

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
concatTokensWithLimit [] limit currentLength = []
concatTokensWithLimit (x : xs) limit currentLength =
  let headLength = length (tokenToString x) - 1
      newLength = headLength + currentLength
   in if currentLength + headLength > limit
        then []
        else (x : concatTokensWithLimit (xs) (limit) (newLength))