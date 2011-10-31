-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 1):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, Mate or some Food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- ALP I - Übungszettel 1

-- Aufgabe I: Gültigkeitsbereiche (10+10 Punkte)


-- Aufgabe II: (10 Punkte)
-- kleinderDurch :: [Integer] -> Int
-- kleinderDurch liste = length [x | x <- liste , (x*(length liste)) < (sum liste)]

--Bricht ab mit:
-- alpa03.hs:16:52:
--     Couldn't match expected type `Integer' with actual type `Int'
--     In the return type of a call of `length'
--     In the second argument of `(*)', namely `(length liste)'
--     In the first argument of `(<)', namely `(x * (length liste))'

-- Aufgabe III: Preisberechnung (10 Punkte)
type Einkaufsliste = [(String, Float)]
type Preisliste    = [(String, Float)]
-- (a)
preis :: Preisliste -> Einkaufsliste -> (Float,[String])
preis preise einkauf = ((gesamtpreis preise einkauf), (nichtvorhanden preise einkauf))

gesamtpreis :: Preisliste -> Einkaufsliste -> Float
gesamtpreis preise einkauf = sum [ (snd x) * (snd y) | x <- preise,	 y <- einkauf, (fst x) == (fst y)]

nichtvorhanden :: Preisliste -> Einkaufsliste -> [String]
nichtvorhanden preise einkauf = [ (fst x) | x <- einkauf, not (elem (fst x) (map fst preise))]

-- (b)
preis2 :: Preisliste -> Einkaufsliste -> (Float,[String])
preis2 preise einkauf = ((gesamtpreis2 preise einkauf), (nichtvorhanden2 preise einkauf))

gesamtpreis2 :: Preisliste -> Einkaufsliste -> Float
gesamtpreis2 preise einkauf = sum [ (snd x) * (snd y) | x <- preise,	 y <- einkauf, (fst x) == (fst y)]

nichtvorhanden2 :: Preisliste -> Einkaufsliste -> [String]
nichtvorhanden2 preise einkauf = [ (fst x) | x <- einkauf, not (elem (fst x) (map fst preise))]


-- Aufgabe IV: Funktionsiteration (10 Punkte)


-- Aufgabe V: Iterierter Logarithmus (10 Punkte)


-- Aufgabe VI: Potenzieren, Summe und Produkt (10 Punkte)


-- Aufgabe VII: Funktionsiteration (10 Punkte)