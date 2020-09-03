
-- 3.2

bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2,3], [], [-100]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3.3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

foo1 :: a -> b -> (a, b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: a -> b -> (b, a)
foo5 = \x -> \y -> (y, x)

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y, x)

f1 :: a -> (a, a)
f1 x = (x, x)

f2 :: (a, b) -> a
f2 (x, _) = x

f3 :: (a, b) -> b
f3 (_, y) = y

f4 :: a -> b -> a
f4 x _ = x

f5 :: a -> b -> b
f5 _ y = y

f :: Int -> Int -> Int
f x y = x + y

g :: (Int, Int) -> Int
g (x, y) = x + y
