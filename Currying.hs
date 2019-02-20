module Currying where


devidedByTen = (/10)


zipWithFunction' :: (a->b->c) -> [a] -> [b] -> [c]
zipWithFunction' _ [] _ = []
zipWithFunction' _ _ [] = []
zipWithFunction' f (x:xs) (y:ys) = (f x y) : (zipWithFunction' f xs ys)


flip' :: (a->b->c) -> b->a->c
flip' f x y = f y x


filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
            | f x = x : filter' f xs
            | otherwise = filter' f xs


addSquares = sum [x*x | x <- [1,2..100], x `mod` 2 /=0, x*x <10000]


doOperation :: (Integral a) => a -> a
doOperation x
        | even x = x `div` 2
        | otherwise = x*3 + 1


doStuff :: (Integral a) => a -> [a]
doStuff 1 = [1]
doStuff xs =  takeWhile (/=1) (xs: doStuff y)
                where y = doOperation xs

