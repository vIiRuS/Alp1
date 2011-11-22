-- -----------------------------------------------------------------------------
-- "THE NERD-WARE LICENSE" (Revision 2):
-- [<viirus@pherth.net>] wrote this file. As long as you retain this notice you
-- can do whatever you want with this stuff. If we meet some day, and you think
-- this stuff is worth it, you can buy me/us a beer, mate or some food in return
-- [Phillip Thelen]
-- -----------------------------------------------------------------------------

-- ALP I - Übungszettel 5

-- Aufgabe 25: Sichtbarkeitsbereiche (5 Punkte)


-- Aufgabe 26: Anonyme Funktionen (λ-Ausdrücke) (15 Punkte)


-- Aufgabe 27: Zinseszinsen (10 Punkte)


-- Aufgabe 28: Abarbeiten von Listen (15 Punkte)


-- Aufgabe 29: Sortieren durch Auswählen (10 Punkte)
ssort :: Eq a => Ord a => [a] -> [a]
ssort [] = []
ssort xs = m:(ssort rest)
    where
        m = foldr (min) (last xs) (init xs)
        rest = delete m xs
 
delete :: Eq a => a->[a]->[a]
delete e (x:xs)
	| e == x = xs
	| otherwise = x:(delete e xs)

-- Aufgabe 30: Verbinden von Listen, Faltung (5 Punkte)


-- Aufgabe 31: Verbinden und Trennen von Listen (20 Punkte)


-- Aufgabe 32: Funktionen höherer Ordnung (10 Punkte)