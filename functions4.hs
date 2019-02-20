lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


addVector :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVector (x1,x2) (y1,y2) = (x1+x2, y1+y2)


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial x = x * factorial(x-1)



tell :: (Show a) => [a] -> String
tell [] = "This list is empty!"
tell [x] = "Only one lement:" ++ show x
tell (x:y:_) = "second:" ++ show y

max' :: (Ord b) => b-> b -> b
max' a b
    | a>=b = a
    | b>a = b


calcBmi :: (RealFloat a) => [(a,a)] -> [a]
calcBmi xs = [bmi w h | (w,h) <- xs]
             where bmi w h = w/ (h ^ 2)


