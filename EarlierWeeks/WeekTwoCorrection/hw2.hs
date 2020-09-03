module HW2
  where



{- B -}

-- Exercise 3.3 from the book
-- What are the types of the following functions?
-- The type has been written right above the definition of each of the functions, with a comment explaining it.

-- Takes a list of 'a' elements and returns an element a.
second :: [a] -> a 
second xs = head (tail xs)

-- Takes a tuple of two elements 'a' and 'b' and returns a tuple of two elements 'b' and 'a', respectively.
swap :: (a,b) -> (b,a) 
swap (x,y) = (y,x)

-- Takes an element of type 'a' and an element of type 'b' and returns a tuple of elements 'a' and 'b', respectively.
pair :: a -> b -> (a,b) 
pair x y = (x,y)

-- Takes of argument a of class Num and returns an element of type a, doubled.
double :: Num a => a -> a
double x = x*2

-- Takes a list of elements of the Eq(uality) types and returns a Boolean.
palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

--  Takes a function and a parameter and returns a value of that function being called twice in succesion.
twice :: (a -> a) -> a -> a
twice f x = f (f x)

{- C -}

-- False :: Bool

-- 5 + 8 :: Num a => a

-- (+)2 :: Num a => a -> a

-- (+2) :: Num a => a -> a

-- (2+) :: Num a => a -> a

-- (["foo", "bar"], 'a') :: ([[Char]], Char) -- because [Char] == String

-- [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

-- \x y -> y !! x :: Int -> [a] -> a -- Int because we are using an array accessor and returning a single element afterwards.

-- [take, drop, \x y -> (y !! x)] :: No type, because of the last element, the lambda function, which does not define the same type as 'take' and 'drop'
-- To further explain, take and drop both have the type 'Int -> [a] -> [a]' but not the lambda function, it only returns an index in the list.
-- Haskell requires every element in a list to have the same type.
-- The lambda function instead has the type 'Int -> [a] -> a'

-- [take, drop, \x y -> [y !! x]] :: :: [Int -> [a] -> [a]]
-- The type is a list of functions which take a list of elements and return a list of elements.

{- D -}
-- Equivalent functions:
-- Type written above definition of each function

foo1 :: a -> b -> (a,b)
foo1 x y = (x,y)

foo2 :: a -> b -> (a,b)
foo2 x = \y -> (x,y)

foo3 :: a -> b -> (a,b)
foo3 = \x y -> (x,y)

foo4 :: a -> b -> (a,b)
foo4 = \x -> \y -> (x,y)

foo5 :: a -> b -> (b, a)
foo5 = \x -> \y -> (y,x)

foo6 :: b -> a -> (b, a)
foo6 = \y -> \x -> (y,x)

-- So the equivalent functions are foo1 === foo2 === foo3 === foo4 == foo6


{- E -}
-- Doing the most simple functions possible (as asked in the question).

f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,y) = x

f3 :: (a,b) -> b
f3 (x,y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y


{- F -}

f :: Int -> Int -> Int
f x y = x+y

g :: (Int, Int) -> Int
g (x,y) = x+y

-- Some tests to show equality between f and g
test1 :: Bool
test1 = (f 1 2) == (g (1,2))

test2 :: Bool
test2 = (f 42 42) == (g (42,42))

test3 :: Bool
test3 = (f (-1) (-34)) == (g ((-1),(-34)))
