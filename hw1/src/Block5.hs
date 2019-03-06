module Block5 where

import Data.Maybe (fromMaybe)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr (\x xs -> fromMaybe [] x ++ xs ) []
