--Task 1
harEl :: (t->Bool) -> [t] -> Bool
harEl _ [] = False
harEl pr (x:xs) = if pr x then True
                  else harEl pr xs


el::(t->Bool)->[t]->t
el pr (x:xs) = if pr x then x
               else el pr xs 


gRep :: (t-> Bool) -> t -> [t] -> [t]
gRep _ _ [] = [] 
gRep pr y (x:xs) = if pr x then y : gRep pr y xs
                  else x : gRep pr y xs 

gRepFasit :: (t-> Bool) -> t -> [t] -> [t]
gRepFasit pr x = map (\y -> if pr y then x else y)

data BT = M Int | N BT Int BT


elt :: BT -> Int -> Bool
elt (M i) x = i == x
elt (N a b c) x = elt a x || elt c x || elt (M b) x

toL :: BT -> [Int]
toL tr = toL' tr []

toL' :: BT -> [Int] -> [Int]
toL' (M i) xs = i : xs
toL' (N a b c) xs = toL' a xs ++ toL' (M b) xs ++ toL' c xs 

dup :: BT -> Bool
dup tr =  dup' (toL tr) []

dup' :: [Int] -> [Int] -> Bool
dup' [] _ = False
dup' (x:xs) alreadyVisited = x `elem` alreadyVisited  ||  dup' xs (x : alreadyVisited) 

dupFasit :: BT -> Bool
dupFasit tr = dupFasit' (toL tr) 

dupFasit' :: [Int] -> Bool
dupFasit' [] = False
dupFasit' (x:xs) = elem x xs || dupFasit' xs 


--Task 2


--naboL ::Eq t => [(t,t)]->[(t,[t])]
--naboL xs = naboL' xs []


--naboL' [] _  = []
--naboL' (k:kl) nl = if harEl ((==(k)).fst) nl then
--                    [(fst x,[snd x]) |  x <- nl] : naboL' kl nl 
 --                  else ("a",["b"]): nl


naboLFasit :: (Eq a, Eq t) => [(t, a)] -> [(t, [a])]
naboLFasit xs = foldr addkFasit [] xs

addkFasit (a,b) nL = if harEl ((== a).fst) nL then
                    let (x,y) = el ((== a).fst) nL in gRep (==(x,y)) (x,b:y) nL
                else (a,[b]):nL

-- Task 2.3

mymain = exc []
--exc :: [(t,t)] -> IO ()
exc gr = do
    putStrLn "g / (k/f) x y / s / q"
    c <- getLine
    let com = words c
    let m = head com
    if m == "q" then return ()
    else if m == "k" || m== "f" then do
            let x = read (head (tail com)) :: Int
                y = read (head (tail (tail com))) :: Int
            if m == "k" then exc ((x,y): gr)
            else exc (filter (/=(x,y)) gr)
    else if m == "g" then exc []
    else if m == "s" then do print ( (naboLFasit gr))
                             exc gr
    else do print "Ukjent kommando"
            exc gr