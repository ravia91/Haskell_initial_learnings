module Folds where
import Data.List (nub, sort)
import qualified Data.Map as M


sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc * x) 0 xs


uniquevals :: (Eq a) => [a] -> [a]
uniquevals xs = nub xs
