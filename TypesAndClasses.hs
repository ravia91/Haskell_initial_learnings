module TypesAndClasses where

data Shape = Circle Float Float Float deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^2



