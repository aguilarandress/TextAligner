module Utils where

-- Recibe una lista de strings y las une mediante
-- un separador
join :: Foldable t => [Char] -> t [Char] -> [Char]
join sep xs = foldr (\a b -> a ++ if b == "" then b else sep ++ b) "" xs