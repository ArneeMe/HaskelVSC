--Oppgave B
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b-> (a,b)
pair x y = (x,y)

double :: Num a => a ->a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t ->t) -> t -> t
twice f x = f (f x)

--Oppgave C

-- 5 + 8 :: Num a -> a
--(+) 2 :: Num a => a -> a
--(+2) :: Num a => a -> a
--(2+) :: Num a => a -> a
--(["foo", "bar"], 'a')  ::  ([[Char, Char]]
--[(True, []), (False, [['a']])]  :: [(Bool, [[Char]])]
-- \x y ->  y !! x  :: Int -> [a] -> a
--[ take, drop, \x y ->  ( y !! x ) ] :: a -> b -> (a,b)
--[ take, drop, \x y ->  [ y !! x ] ] :: [Int -> [a] -> [a]]

--Oppgave D
--foo1 til og med foo4  er like, mens foo5 og foo6 er også ekvivalente
foo1 :: a -> b -> (a,b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a,b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a,b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a,b)
foo4 = \x -> \y -> (x, y)

foo5 :: a -> b -> (b,a)
foo5 = \x -> \y -> (y,x)

foo6 :: b -> a -> (b,a)
foo6 = \y -> \x -> (y,x)


--Oppgave E

f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,y) = fst(x,y)

f3 :: (a,b) -> b
f3 (x,y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 = \x y -> y

--Oppgave F
f :: Int -> Int -> Int
f x y = x+y
g :: (Int,Int) -> Int
g (x,y) = x+y
