module DataTypes where

import Data.Map (Map)

data Token = Word String | Blank | HypWord String
  deriving (Eq, Show)

type HypMap = Map String [String]

type Line = [Token]