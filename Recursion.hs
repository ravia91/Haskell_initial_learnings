module Recursion where

factorial xs = case xs of 0 -> 1
                          1 -> 1
                          _ -> factorial(xs-1) + factorial(xs-2)


maximum' xs = case xs of   [] -> error "List is Empty"
                           xs:[] -> xs
                           (x:y) -> if x > maxTail y then x else maxTail y
                           where maxTail x1= maximum' x1

replicate' 0 _ = []
replicate' x y = if x<0 then error "Negative number not valid"
                else y:(replicate' (x-1) y)


take' 0 _ = []
take' x y = if x > length y then y
            else remainingVal x y
            where remainingVal y1 (x:xs) = x:(take (y1-1) xs)




quicksort [] = []
quicksort (x:xs) = let smallerValues x1= quicksort [ x1 | x1 <- xs , x1<=x]
                       largerValues y1 = quicksort [ y1 | y1 <-xs, y1>x]
                   in smallerValues xs ++ [x] ++ largerValues xs

mergesort [] = 0
mergesort [x] = 1
mergesort xs = let halflen x1 = (length x1) `div` 2
               in halflen xs


