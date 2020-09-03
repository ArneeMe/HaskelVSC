--Seksjon B
--1.3
product1 [] = 1 
product1 (x:y) = x * product1 y

--1.4
--We take the sort function from the book, and we reverse the larger and smaller so it 
--produses a list that is from largest to smallest. 
qsortReverse [] = []
qsortReverse (x:xs) = qsortReverse larger ++ [x] ++ qsortReverse smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x ]

--1.5
qsort1 [] = []
qsort1 (x:xs) = qsort1 smaller ++ [x] ++ qsort1 larger
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x ]
qsort2 [] = []
qsort2 (x:xs) = qsort2 smaller ++ [x] ++ qsort2 larger
    where
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x ]
--If we remove the "="-sign the qsort2 function will not look for multiple of the same number,
--Like the qsort1 does.
--This is because while the qsort function goes through the list it does not check for the equal
--number that it just sorted, because the equality sign is removed.

--2.4
findLastItem xs = drop(length xs-1) xs !! 0 
--2.5
removeLastItem xs = take(length xs-1) xs 

--Seksjon C
--plu [] n = []
--plu (k:ks) n = (k + n) : plu()
-- ??