--B

--1.4

qsortr [] = []

qsortr (x:xs) = qsortr smaller ++ [x] ++ qsortr larger
                where 
                    larger = [a | a <- xs, a <= x]
                    smaller = [b | b <- xs, b > x]
                    
-- Switch the place of the variables larger/smaller

--1.5

qsort [] = []

qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                    smaller = [a | a <- xs, a < x]
                    larger = [b | b <- xs, b > x]
                    
-- Duplicate numbers replaces each other? 



--2.4

--Select the last element of a list

last1 as = as !! (length as - 1)

--2.5

init1 xs = reverse (tail(reverse xs))

init2 xs = take(length xs - 1) xs

--C

--1)

plu :: ([Int], Int) -> [Int]
plu (xs, n) = [x+n | x <- xs]

--2)

--pali

pali :: Eq(a) => [a] -> Bool
pali xs = xs == (reverse xs)

