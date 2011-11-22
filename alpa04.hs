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

length'' xs = sum(map (\x -> 1) xs)
length''' xs = sum.map(const 1)

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

= b - ((b+1) - ((b+2) - ( ... (c - a) .. )))

(1) Für (c-b+1)/(#Elemente) gerade
	foldr (-) a [b, b+1]
	= b - ((b+1) - a)
	= a - 1
	foldr (-) a [b, b+1, b+2, b+3]
	= ... = a - 2 = (#Elemente)/2
(2) Für c - b + 1 ungerade
	foldr (-) a [b, b+1, b+2]
	= b - ((b+1) - ((b+2) - a))
	= b - ( -1 + a)
	= b + 1 - a
	foldr (-) a [b, .., b+4]
	= ... = b+2-a = (#Element)/2

Vermutung:  Für c-b+1 gerade ist d a b c = a - (c-b+1)/2
			Für c-b+1 ungerade ist d a b c = b + ((c - b + 1)/2) - a

IB: foldr (-) a [b..c] = a - (c-b+a)/2 -- für c-b+1 gerade
IA: foldr (-) a [b, b+1]
	= a - 1 = a - (c-b+1)/2
IV: IB giltfür ein c-b+a gerade
IS: [b..c] -> [b-2..c]
	foldr (-) a [b-2..c]
	= (b-2) - ((b-1) - foldr (-) a [b..c])
	= (b-2) - ((b-1) - (a - (c-b+1)/2)) = (b-2) - (b-1 - a - (c-b+1)/2) = -1 + a - (c-b+1)/2
	= -2/2 + a - (c-b+1)/2 = a - (c-b+1+2)/2 = a - (c - (b-2) +1)/2


-- Aufgabe 23: Strukturelle Induktion (40 Punkte)
-- (a)
-- IB: map f (a ++ b) = map f a ++ map f b
-- Beweis nach a:

-- IA: a=[]
-- Linke Seite:
-- map f ([] ++ b)
-- = map f b -- ++.1
-- Rechte Seite:
-- map f [] ++ map f b
-- = [] ++ map f b -- m.1
-- = map f b -- ++.1

-- IV: IB gilt für eine Liste a

-- IS: a=(x:xs)
-- linke Seite:
-- map f ((x:xs)++b)
-- = map f (x:(xs++b) -- ++.2
-- =f x:(map f (xs++b)) -- m.2
-- Rechte Seite:
-- map f (x:xs) ++ map f b
-- = f x:(map f xs) ++ map f b -- m.2
-- = f x:(map f xs ++ map f b)
-- = f x:(map f (xs++b)) -- IV

-- Reihenfolge : strukturelle Induktion:
-- I.Behauptung
--   v
-- I.Anker / I.Basis
--   v
-- I.Vorraussetzung / I.Annahme
--   v
-- I.Schritt




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
