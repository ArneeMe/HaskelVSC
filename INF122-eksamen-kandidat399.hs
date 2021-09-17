-- 399
--Stay calm, and haskell on
--Oppgave 1 Evaluering av foldl og foldr
-- Her vist på to ulike illustrende måter

-- foldl :: (a-> b -> a) -> a -> [b] -> a
-- foldl (-) 0 [2,4,6] gir:
-- foldl (-) (0 - 2) [4,6])
-- foldl (-) ((0-2)-4) [6]
-- foldl (-) (((0-2)-4)-6) []     Når liste er tom stopper foldl og vi får
-- ( ( 0 - 2) -4 ) - 6
-- Som evalures innerst i parentesene først:
-- ( ( - 2 ) - 4 ) -6
-- (-6) -6
-- -12

-- Se også scanl for de midlertidige resultatene

-- foldr (a -> b -> b) -> b -> [a] -> b
-- Fungerer tilsynelatene likt som foldl men kort sagt setter parentesene fra høyre og ikke venstre
-- foldr (-) 0 [2,4,6] gir:
-- 2 - (foldr (-) 0 [4,6])
-- 2 - (4 - (foldr (-) (0) [6]))
-- 2 - (4 - (6 -( foldr (-) (0) [] )))
-- 2 - (4 - (6 - (0)))
-- som evalures:
-- 2 - (4 - (6))
-- 2 - ( - 2)
-- 2 + 2 
-- 4

-- Så selv om foldl og foldr gir samme resultat i en (+) operasjon gjør det svært forskjellige ting i en (-) operasjon
-- Her og viser scanr de midlertidige resultatene i en liste


-- Oppgave 2

-- Oppgave 2.1
funk :: Eq a => [a] -> Bool
funk [] = True
funk (x:xs) = let com = everyOther xs in not (elem x com) && funk xs

everyOther :: [a] -> [a]
everyOther xs = case drop 1 xs of
              (y:ys) -> y : everyOther ys
              [] -> []

-- Oppgave 2.2

avb :: Eq a => [a] -> [a] -> [a]
avb str [] = str
avb str [_] = str
avb str (a:b:ls) = if funk str then avb (map(\x -> if a == x then b else x) str) ls
                   else str
--Liten feil her siden den tar ikke inn ls = "b"  og bytter ut alle b-er med en ""
--


-- Oppgave 2.3
fle :: Eq a => [a] -> [a] -> [a]
fle xs [] = xs
fle [] ys = ys
fle (x:xs) (y:ys) = x : y : fle xs ys

-- Oppgave 2.4
bin :: (Int -> Int -> Int) -> String -> String -> String
bin pr x y =show(mybin pr x y) 


mybin (+) x y = read(x) + read(y) :: Int ----GHCI er ikke fornøyd med at jeg bruker den predefinerte funksjonen +
mybin (-) x y = read(x) - read(y) :: Int -- Men ønsker å bruke det på den måten slik at det gjør programmet riktig
mybin (^) x y = read(x) ^ read(y) :: Int


-- Oppgave 2.5

umin :: String -> String
umin x = umin' ((read x) :: Int)

umin' a = if a > 0 then "-" ++ show(a)
          else show(abs(a))
-- Oppgave 3

-- Vi har følgende grammatikk:
-- E ::= Tall|Ord|* E E *|+ E E +|– E
--Tall ::= Digit|Digit Tall
--Ord ::= Bokstav|Bokstav OrdDigit::= 0|1|...  8|9
--Bokstav::= a|A|b|B ...  z|Z
--Kort kommentar, her ser vi at det er lov å addere og multiplisere tall og ord 

--eva :: String -> (String, String)

--Har veldig dårlig tid men vet hvordan jeg skulle ha løst oppgaven
--Her kan vi benytte oss mye av tidligere funksjoner.
-- Det første vi gjør er å lage en funksjon som skiller operatorene med verdiene
-- Så bruker vi flettefunksjonen (fle) sammen med foldl fra første oppgave sammen med paranteser 
-- På denne måten vi får det på riktig format, og så kan vi bruke bin funksjonen i vært ledd for å få et evalurert utrykk
-- Merk og at grammatikken spesifiserer at vi ikke kan ta to ting minus hverandre, men bare negative utrykk, da bruker vi umin funksjonen til det

-- eva :: String ->  (String, String)
 
-- Oppgave 3.2

-- Oppgave 3.3
-- Oppgave 3.4

-- Oppgave 4

-- Vi har:
--f x 0 = 0
--f x y = (x * y) + f x (y-1)

-- Dette er en funksjon som kjører rekursivt på seg selv, og for vært ledd så tar den produktet av x og y, og kjører til y =0
-- Eksempelkjøring: f 3 2: (3*2)  + (f 3 1) = (3*2) + (3*1) + f(3 0 ) = (3*2) + (3*1) + (3*0) = 9

myLambda = doAgain (\x -> \y ->  if y == 0 then 0 else (x*y) + doAgain(y-1) ) --Error, does not stop
doAgain y = doAgain (y)


-- Oppgave 5

-- Vi har 4 ulike regler for å å avlede typer i haskell. Her bruker jeg de for å finne riktig type:
-- \x -> x (\y ->\z -> y z) 
-- E (Ø | \x -> x (\y ->\z -> y z) :: t)                                                                (t4)
--E (x :: a | \x -> x (\y ->\z -> y z ::b) :: t = a -> b)                                               (t4)
--E (x :: a y :: c  | \x -> x (\y ->\z -> y z ::d ) :: {b = c ->d ,t = a -> b})                         (t4)  
--E (x :: a y :: c z :: e | \x -> x (\y ->\z -> y z ::f ) :: {d = e -> f, b = c ->d ,t = a -> b})       (t4)
--E (x :: a y :: c z :: e | \x -> x (\y ->\z -> g ::f ) :: {y z -> g, d = e -> f, b = c ->d ,t = a -> b}) (t3)
--E (x :: a y :: c z :: e | \x -> x (\y ->\z -> g ::f ) :: {y z -> g, d = e -> f, b = c ->d ,t = a -> b}
-- Vil til slutt bli feil for den vil ikke være fornøyd med slik du har skrevet lambda y


-- Oppgave 6
-- (++) :: [a] -> [a]
--Her kan vi bruke induksjonsbevis for å bedømme at casen der baade xs og ys er null 
--Også vil vi og se, ved hjelp av rekursjon, at tillfellet "n-te + 1", der man legger til et element
--også vil være det samme