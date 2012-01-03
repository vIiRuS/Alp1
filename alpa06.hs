-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 2):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, mate or some food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- ALP I - Übungszettel 6

-- Aufgabe 33: (8 Punkte)
-- (a)
filtera :: (a->Bool) -> [a] -> [a]
filtera _ [] = []
filtera p (x:xs)
	| p x = x:filtera p xs
	| otherwise = filtera p xs

-- (b)
filterb :: (a->Bool) -> [a] -> [a]
filterb p xs = [x | x <- xs, p x]

-- (c)
loeschleer :: [Char] -> [Char]
loeschleer xs = filter (/= ' ') xs


-- Aufgabe 34: Auswertungsstrategien (10 Punkte)



-- Aufgabe 35: (10 Punkte)




-- Aufgabe 36: Darstellung von Ausdrücken (10 Punkte)




-- Aufgabe 37: Strenge Funktionen (7 Punkte)
-- für doppel n a = a*a -- nur a
-- für f1 n a = if n==0 then a+1 else a-n -- n und a
-- für f2 n a = in n==0 then a+1 else n -- n steng (a nur bedingt)

-- Aufgabe 38: Träge Multiplikation (5 Punkte)
mult :: Num a => a -> a -> a
mult x y
	| x == 0 = 0
	| otherwise = x * y


-- Aufgabe 39: Spiegeln von zweidimensionalen Mustern (10 Punkte)
-- flipH :: String -> String

-- flipV :: String -> String

-- Aufgabe 40: Die Fibonacci-Zahlen (10 Punkte)
-- (a)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)


-- Aufgabe 41: Kodierung von Bäumen (5 Punkte)

data Baum' a = Blatt a
			| Knoten a (Baum' a) (Baum' a)

baumk = Knoten 27 (Knoten 5 (Blatt (-2)) (Blatt 7)) (Knoten 44 (Blatt 8) (Knoten 5 (Blatt 7) (Blatt 9)))

baumkodierung :: Baum' Int -> String
baumkodierung (Blatt a) = (show a)
baumkodierung (Knoten a x1 x2) = (show a) ++ "D" ++ (baumkodierung x1) ++ "U" ++ "D" ++ (baumkodierung x2) ++ "U"

