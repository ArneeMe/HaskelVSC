--We take the sort function from the book, and we reverse the larger and smaller so it 
--produses a list that is from largest to smallest. 
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x ]