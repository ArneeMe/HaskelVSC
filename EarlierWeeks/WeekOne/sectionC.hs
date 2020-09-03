plu :: [Int] -> Int -> [Int]
plu [] _ = []
plu (k:ks) n = (k + n) : plu (ks) n

--pali ::Eks [a] -> Bool