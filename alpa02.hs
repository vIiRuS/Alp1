-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 1):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, Mate or some Food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- Aufgabe 1
zeitdifferenz :: (Int, Int) -> (Int, Int) -> (Int, Int)
zeitdifferenz (h1, m1) (h2, m2) = inStunden (inMinuten h2 m2 - inMinuten h1 m1)

inMinuten :: Int -> Int -> Int
inMinuten h m = h*60 + m

inStunden :: Int -> (Int, Int)
inStunden m = (div m 60, mod m 60) 

-- Aufgabe 2
zeitConvert :: (Int, Int) -> String
zeitConvert (h, m)
	| h == 0 && m == 0 = "12:00 midnight"
	| h == 0 = "12:" ++ show (m) ++ " a.m."
	| h == 12 && m == 0 = "12:00 noon"
	| h < 12 = show (h) ++ ":" ++ show (m) ++ " a.m."
	| h == 12 = "12:" ++ show (m) ++ " p.m."
	| otherwise = show (h-12) ++ ":" ++ show (m) ++ " p.m."


-- Aufgabe 3
multitabelle :: IO ()
multitabelle = putStr ("   |   1   2   3   4   5   6   7   8   9  10\n---+----------------------------------------\n" ++ tabellenreihe 1)

tabellenreihe :: Int -> String
tabellenreihe 11 = ""
tabellenreihe x = stringspaces 2 (show x) ++ " |" ++ position x 1 ++ "\n" ++ tabellenreihe (x+1)

position :: Int -> Int -> String
position _ 11 = ""
position x y = stringspaces 4 (show (x*y)) ++ position x (y+1)

stringspaces :: Int -> String -> String
stringspaces len str
	| length str == len = str
	| otherwise = stringspaces len (" " ++ str)

-- Aufgabe 4
test x y z
	| x <= y    = True
	| y <= z    = False
	| otherwise = x < z

test2 :: Integer -> Integer -> Integer -> Bool
test2 x y z = if x <= y then True else False


-- Aufgabe 5
-- (a)
teilerliste :: Integer -> [Integer]
teilerliste zahl = teiler zahl (div zahl 2) []

teiler :: Integer -> Integer -> [Integer] -> [Integer]
teiler zahl 1 liste = 1:liste
teiler zahl teil liste
	| mod zahl teil == 0 = teiler zahl (teil-1) (teil:liste)
	| otherwise = teiler zahl (teil-1) liste

-- (b)

zahlUnter :: [Integer]
zahlUnter = unterSumme 2 []

unterSumme :: Integer -> [Integer] -> [Integer]
unterSumme 1001 liste = liste
unterSumme x liste
	| x < sum (teilerliste x) = unterSumme (x+1) (liste ++ [x])
	| otherwise = unterSumme (x+1) (liste)

-- (c)

zahlPerfekt :: [Integer]
zahlPerfekt = perfekteZahlen 2 []

perfekteZahlen :: Integer -> [Integer] -> [Integer]
perfekteZahlen 1001 liste = liste
perfekteZahlen x liste
	| x == sum (teilerliste x) = perfekteZahlen (x+1) (liste ++ [x])
	| otherwise = perfekteZahlen (x+1) (liste)

-- Aufgabe 5 (Friedrich)
-- a
aufg5 :: Int -> [Int]
aufg5 n = [a | a <- [1..(n-1)], mod n a == 0]

-- b
liste :: [Int]
liste = [z | z <- [1..1000], sum (aufg5 z) > z]

-- c
liste2 :: [Int]
liste2 = [a | a <- [1..1000], sum (aufg5 a) == a]
