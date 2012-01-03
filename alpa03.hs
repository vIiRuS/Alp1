-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 1):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, Mate or some Food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- ALP I - Übungszettel 3

-- Aufgabe 12: Gültigkeitsbereiche (10+10 Punkte)
-- (a) (Nur teilweise gelöst!)
x = 25:: Integer -- (x: global)
biggerThanAVG3:: Integer->Integer->Integer->Integer
biggerThanAVG3 x y z = -- (biggerThanAVG3: global, x, y, z: innerhalb dieser Funktion)
            sum (map (\x -> if fromIntegral x > avg3 x y z then 1 else 0) -- (x: innerhalb der anonymen Funktion)
                     [x ,y, z]) -- (x, y, z: Wert aus der Funktion)
            where avg3:: Integer->Integer->Integer->Double
                  avg3 a b c = fromIntegral (a+b+c) / 3 -- (avg3: innerhalb der Funktion, a, b, c: innerhalb von avg3)
test1:: Integer
test1 = biggerThanAVG3 3 4 5 -- (test1: global)
fläche:: Float -> Float
fläche x = 2*x^2*pi -- (fläche: global, x: innerhalb der Funktion)

-- (b) (Nur teilweise gelöst!)
f x y = -- (f: global, x, y: innerhalb der Funktion)
	let n = 3 in take n (g y) ++ take n (g x) -- 
	where g x = take n xys --(g: innerhalb der Funktion, n: innerhalb von g)
			where
				xys = [x] ++ yxs -- (xys: innerhalb von g)
				yxs = [y] ++ xys -- (yxs: innerhalb von g)
				n = 10 -- (n: innerhalb der Funktion)


-- Aufgabe 13: (10 Punkte)
kleinderDurch :: [Int] -> Int
kleinderDurch liste = length [x | x <- liste , (x*(length liste)) < (sum liste)]


-- Aufgabe 14: Preisberechnung (10 Punkte)
type Einkaufsliste = [(String, Float)]
type Preisliste    = [(String, Float)]
-- (a)
preis :: Preisliste -> Einkaufsliste -> (Float,[String])
preis preise einkauf = ((gesamtpreis preise einkauf), (nichtvorhanden preise einkauf))

gesamtpreis :: Preisliste -> Einkaufsliste -> Float
gesamtpreis preise einkauf = sum [ (snd x) * (snd y) | x <- preise, y <- einkauf, (fst x) == (fst y)]

nichtvorhanden :: Preisliste -> Einkaufsliste -> [String]
nichtvorhanden preise einkauf = [ (fst x) | x <- einkauf, not (elem (fst x) (map fst preise))]

-- (b)
preis2 :: Preisliste -> Einkaufsliste -> (Float,[String])
preis2 preise einkauf = ((gesamtpreis2 preise einkauf), (nichtvorhanden2 preise einkauf))

gesamtpreis2 :: Preisliste -> Einkaufsliste -> Float
gesamtpreis2 preise einkauf = sum [ (fromIntegral (round ((snd x)*100))/100) * (snd y) | x <- preise, y <- einkauf, (fst x) == (fst y)]

nichtvorhanden2 :: Preisliste -> Einkaufsliste -> [String]
nichtvorhanden2 preise einkauf = [ (fst x) | x <- einkauf, not (elem (fst x) (map fst preise))]


-- Aufgabe 15: Funktionsiteration (10 Punkte)
iter n f x
	| n==0 =x
	| n>0   = f (iter (n-1) f x)
-- (a)
-- Der Typ von iter ist abhängig von den Typen von n, vom Typ der Funktion, und von x
-- iter :: Typ von n -> Typ von f (wobei f :: Typ von x) -> Typ von x

-- (b)
iter2 :: Int -> (a -> a) -> a -> a
iter2 n f
      | n == 0 = id
      | n > 0  = f.(iter (n-1) f)

-- (c)
-- zinsen :: Int -> Int -> Float
zinsen kapital zinsfuß = kapital * zinsfuß * 0.01 -- siehe alpa01.hs Aufgabe 4 (a)

-- endwert :: Int -> Int -> Int
endwert zinsfuß kapital = kapital + zinsen kapital zinsfuß -- siehe alpa01.hs Aufgabe 4 (a)

-- zinsiter :: Int -> Int -> Int
zinsiter kapital zinsfuß = iter 2 (endwert zinsfuß) kapital


-- Aufgabe 16: Iterierter Logarithmus (10 Punkte)
-- (a)
smallLogBase1 :: Float -> Float
smallLogBase1 x
	| logBase 2 x >= 5 = x
	| otherwise = smallLogBase1 (x+1)
-- -> 5

-- (b)
smallLogBase2 :: Float -> Float
smallLogBase2 x
	| iter 3 (logBase 2) x >= 2 = x
	| otherwise = smallLogBase2 (x+1)
-- -> 65536


-- Aufgabe 17: Potenzieren, Summe und Produkt (10 Punkte)
potenz x n = iter n (x*) 1

-- (a)
turm :: Integer -> Integer -> Integer
turm x k = iter (k-1) (potenz x) x

-- (b)
mal :: Integer -> Integer -> Integer
mal a b = iter (b-1) (a+) a

-- (c)
-- Man muss (+) iterieren
plus a b = iter 1 (a+) b
-- alternativ:
plus2 a b = iter a (1+) b
-- das erste ist effizienter, wobei das zweite auch "korrekt" iteriert.

-- Aufgabe 18: Funktionsiteration (10 Punkte)
-- (a)
g = iter 23

entdecke :: (Num a, Num b) => ((a -> a) -> b -> c) -> c
entdecke f = f (1+) 0

-- (b)
sumiter :: (a -> b -> c) -> (a -> c -> d) -> a -> b -> d
sumiter f g h = (g h).(f h)

-- (c)
proditer :: (b -> c) -> (a -> b) -> a -> c
proditer = (.)
