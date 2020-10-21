module DataTypes where

data Token = Word String | Blank | HypWord String
  deriving (Eq, Show)

type Line = [Token]