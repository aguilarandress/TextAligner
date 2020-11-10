module DataTypes where

import Data.Map (Map)

data Token = Word String | Blank | HypWord String
  deriving (Eq, Show)

type HypMap = Map String [String]

data BanderaAjustar = AJUSTAR | NOAJUSTAR
  deriving (Eq, Show)

data BanderaSeparar = SEPARAR | NOSEPARAR
  deriving (Eq, Show)

type Line = [Token]