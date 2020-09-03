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