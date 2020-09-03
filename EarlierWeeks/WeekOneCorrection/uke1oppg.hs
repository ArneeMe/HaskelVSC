-- 1.4 How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?

-- Answer: switching the order of larger and smaller in the list will give reverse sorted list       
qsortrev [] = []
qsortrev (x:xs) = qsortrev larger ++ [x] ++ qsortrev smaller
    where
       smaller = [a | a <- xs, a <= x]
       larger = [b | b <- xs, b > x]

-- 1.5 What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1]
-- Repeated elements will be deleted from the list. The example will be sorted to [1,2,3]


-- 2.4 The library function last selects the last element of a non-empty list; for example, last [1,2,3,4,5] = 5. Show how the function last could be defined in terms of the other library functions introduced in this chapter. Can you think of another possible definition?
last1 xs = xs !! (length xs -1)
last2 xs = head (reverse xs)


--2.5 The library function init removes the last element from a non-empty list; for example, init [1,2,3,4,5] = [1,2,3,4]. Show how init could similarly be defined in two different ways.
init1 [x] = []
init1 (x:xs) = x : (init xs)
init2 xs = take (length xs-1)xs


{- C. Programmer følgende funksjoner:

1. plu: [Int] -> Int -> [Int]
   Input: en liste k med heltall og et heltall n
Output: listen der hvert element e fra listen k er erstattet med e+n (elementene står i samme rekkefølge som i k). -}

-- Answer: 

plu [] n = []
plu (k:ks) n = [k + n] ++ (plu ks n)


{- 2. pali:: [a] -> Bool
som gir True hvis inputlisten er en palindrome og False ellers.
(== er boolsk likhet, dvs. s == t gir True hvis s og t er like, og False ellers.)  -}

--Answer:
pali :: Eq a => [a] -> Bool
pali xs = xs == reverse xs