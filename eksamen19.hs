import Data.Char ( isDigit, isAlpha, isSpace) 
--Task 1.2
nats1 :: [Integer]
nats1 = 0:1:tail nats1 -- [0,1,1,1,1] Riktig

nats2:: [Integer]
nats2 = 0:tail nats2 -- [0,0,0,0,0] Feil, dette blir error

nats3 :: [Integer]
nats3 = 0:map (+1) nats3 -- [1,2,3,4,5] Delvis feil (Dette blir [0,1,2,3,4])


nats4 :: [Integer]
nats4 = map (+1) [0..] -- [1,2,3,4,5] Riktig

--Task 1.3
-- Evauler denne:  ["ab","cd","","efg"]
concat1 xss = [x | x<-xss] -- ["ab","cd","","efg"] Riktig
concat2 xss = [x | xs<-xss, x<-xs] --error? Feil, den blir  "abcdefg"
concat3 xss = concat (tail xss) --verdien vil ikke bli lagret vil den? Riktig
concat4 xss = map (++) xss --["abcdefg"] Feil, den blir  [(++)"ab",(++)"cd",(++)"",(++)"efg"]


--Task 1.4
--apply f x = f x, har enn av disse typene:

--a -> b -> c 
--(a -> b) -> a -> b Jeg tror denne, Riktig
--a -> (b -> a) -> b
--a -> b -> (a -> b)

--Task 2, matrisemultiplikasjon

row :: [[Int]] -> Int -> [Int]
row m r = m !! (r-1)

col :: [[Int]] -> Int -> [Int]
col [] _ = []
col (x:xs) k = x !! (k-1) :  col xs k

cols :: [[Int ]] -> [[Int]]
cols xss =  map (col xss) [1..length xss]

--mult :: [[Int]] -> [[Int]] -> [[Int]]
--mult n m = n  cols(m)

--Task 3 IO

--E ::= Pos | E E * | E E + | E E –
--Pos ::= Digit | DigitPos
--Digit ::= 0 | 1 | ... 8 | 9


--eval :: IO()
eval = myMain []

myMain :: [Int] -> IO ()
myMain xs = do
    ja <- getLine
    let c = filter (/= " ") ja
    if isDigit (head c) then do     let hei = ((read c) ::Int) : xs
                                    print(hei)
                                    myMain (hei)
    else
        eval
 
     

--myisDigit :: a -> Bool
myisDigit :: (Eq a, Num a, Enum a) => a -> Bool
myisDigit x  = elem x [1..99] 