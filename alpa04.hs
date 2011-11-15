-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 2):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, mate or some food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- ALP I - Übungszettel 4

-- Aufgabe 19: Funktionen höherer Ordnung (5 Punkte)
length' :: [a] -> Int
length' xs = sum (map repl xs)

repl :: a -> Int
repl x = 1

-- Aufgabe 20: Listenfunktionen (10 Punkte)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
	| p x = x:(takeWhile' p xs)
	| otherwise = []

splitAt' :: Int -> [a] -> ([a],[a])
splitAt' n xs  =  (take n xs, drop n xs) 

-- Aufgabe 21: (5 Punkte)

-- map :: (Int -> Int -> Float) -> [Float] -> [Float]
-- macht das Sinn?

-- Aufgabe 22: Nichtassoziative Faltung von Listen (10 Punkte)
differenzen :: Integer -> Integer -> Integer -> Integer
differenzen a b c = foldr (-) a [b..c]


-- Aufgabe 23: Strukturelle Induktion (40 Punkte)
-- (a)
-- Induktionsbasis
-- map f [] ++ b = map f [] ++ map f b

-- Induktionsaussage
-- map f (xs ++ b) = map f xs ++ map f b

-- Induktionsbehauptung
-- map f (x:xs) ++ b = map f (x:xs) ++ map f b

-- linke Seite
-- map f (x:xs) ++ b
-- = map f x:(xs ++ b)
-- = [f x] ++ map f (xs ++ b)
-- [f x] ++ map f xs ++ map f b

-- rechte Seite
-- [f x] ++ map f xs ++ map f b


-- (b)
-- foldr g z (a ++ b)
-- = foldr g (foldr g z b) a


-- Aufgabe 24: Lauflängenkodierung (run-length encoding)
-- (a) (10 Punkte)
kodieren :: [Char] -> [(Int, Char)]
kodieren [] = []
kodieren (x:xs) = (cutlength+1, x):kodieren (drop cutlength xs)
	where cutlength = length (takeWhile (== x) xs)

dekodieren :: [(Int, Char)] -> [Char]
dekodieren [] = ""
dekodieren ((n, x):xs) = (replicate n x) ++ dekodieren xs

-- (b) (10 Punkte)
kodieren2 :: [Char] -> [Char]
kodieren2 [] = []
kodieren2 (x:xs) = ( show (cutlength+1) ++ [x]) ++ kodieren2 (drop cutlength xs)
	where cutlength = length (takeWhile (== x) xs)

dekodieren2 :: [Char] -> [Char]
dekodieren2 [] = ""
dekodieren2 (x:xs) = (replicate (read [x]) (head xs)) ++ dekodieren2 (tail xs)
