mengde :: Eq t => [t] -> Bool
mengde [] = False
mengde (x:xs) = elem x xs || mengde xs

rep :: Eq t => [t] -> [t]
rep [] = []
rep (x:xs) = if elem x xs then rep xs
             else x : rep xs


del :: Eq t => [t] -> [t] -> Bool
del [] _ = True
del (x:xs) ys = elem x ys && del xs ys


eq :: Eq t => [t] -> [t] -> Bool
eq xs ys = del xs ys && del ys xs

--eqG :: (t->t->Bool) -> [t] -> [t] -> Bool

--eqG pr (x:xs) = map (\y -> if pr y then True else False)

eqGFasit :: (t1 -> t1 -> Bool) -> [t1] -> [t1] -> Bool
eqGFasit pr xs ys = delG pr xs ys && delG pr ys xs


delG _ [] _ = True
delG pr (x:xs) ys = or [pr x y | y <- ys] && delG pr xs ys

--eqGJorn = (((and ) . (<*>)) .) . (<$>)

snuK :: [(t,t)] -> [(t,t)]
snuK kl = [(x,y)| (y,x)<- kl]

--snuN :: [(t,[t])] -> [(t,[t])]
snuN xs = let kL = convNK xs in map (collectOne kL) (noder kL)
convNK nl = concat (map oneN nl) 
oneN (a,ns) = map (\x -> (x,a)) ns 
noder kls = rep ([z | (x,z) <- kls] ++ [x | (x,z) <- kls])
collectOne kl x = (x,[z | (y,z) <- kl, y==x]) 



naboer :: Eq t => [(t,[t])] -> t -> [t]
naboer nL x = let ls = [el | el <- nL, fst el == x] in
                if null ls then [] else snd (head ls)


reach :: Eq t => [(t, [t])] -> t -> [t]
reach nL x = rep (concat (trav nL [] x))
trav nL vis x = if (elem x vis) then [vis]
                else let reca = naboer nL x in
                    if null reca then [x:vis]
                    else concat (map (trav nL (x:vis)) reca)